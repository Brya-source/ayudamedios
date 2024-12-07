import threading
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.service import Service
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException, NoSuchElementException, WebDriverException
from webdriver_manager.chrome import ChromeDriverManager
import mysql.connector
import time
from time import sleep
import random
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.firefox.service import Service as FirefoxService
from webdriver_manager.firefox import GeckoDriverManager

# Configuramos las opciones del navegador
opts = Options()
opts.add_argument(
    "user-agent=Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
opts.page_load_strategy = 'eager'
opts.add_argument('--disable-blink-features=AutomationControlled')

# Inicializamos el driver de Firefox
driver = webdriver.Firefox(service=FirefoxService(GeckoDriverManager().install()))
# STenemos el driver de chrome por si acaso
# driver = webdriver.Chrome(service=Service(ChromeDriverManager().install()), options=opts)
driver.set_page_load_timeout(30)

# Configuración de la conexión a la base de datos
db_config = {
    'user': 'root',
    'password': 'Soccer.8a',
    'host': '127.0.0.1',
    'database': 'noticias3',
    'raise_on_warnings': True
}


def cerrar_mensaje_cookies(driver):
    """Intentamos cerrar el mensaje de cookies si aparece."""
    try:
        boton_cookies = WebDriverWait(driver, 10).until(
            EC.element_to_be_clickable((By.ID, 'btnAceptarCookies'))
        )
        boton_cookies.click()
        print("Hemos cerrado el mensaje de cookies.")
    except Exception as e:
        print(f"No pudimos cerrar el mensaje de cookies: {e}")


def extraer_noticia(driver, link, cursor, conn):
    """Extraemos la información de una noticia específica."""
    original_window = driver.current_window_handle  # Guardamos la ventana principal

    try:
        # Abrimos la noticia en una nueva pestaña
        driver.execute_script("window.open(arguments[0]);", link)
        driver.switch_to.window(driver.window_handles[-1])  # Cambiamos a la nueva pestaña
        driver.set_page_load_timeout(30)

        # Esperamos un momento para que la página cargue completamente
        time.sleep(4)
        sleep(5)

        # Extraemos la información de la noticia
        titulo = driver.find_element(By.XPATH, '//h1').text
        descripcion = driver.find_element(By.XPATH, '//h2').text
        sleep(3)
        texto = driver.find_element(By.XPATH, '//section').text
        fecha = driver.find_element(By.XPATH, '//span[contains(@class, "sc__author--date")]').text
        try:
            autor = driver.find_element(By.XPATH, '//div[@class="sc__author-nota font-bold text-lg"]').text
        except NoSuchElementException:
            autor = "Autor desconocido"

        # Eliminamos texto no deseado
        texto_no_deseado = "Únete a nuestro canal ¡EL UNIVERSAL ya está en Whatsapp!"
        if texto_no_deseado in texto:
            texto = texto.split(texto_no_deseado)[0]

        patrones_no_deseados = ["Lee también", "Lea también", "Leer también", "También lee"]
        for patron in patrones_no_deseados:
            while patron in texto:
                index_inicio = texto.find(patron)
                index_final = texto.find("\n", index_inicio)
                if index_final != -1:
                    texto = texto[:index_inicio] + texto[index_final:]

        # Mostramos los resultados de la noticia
        print(f"Título: {titulo}")
        print(f"Descripción: {descripcion}")
        print(f"Fecha: {fecha}")
        print(f"Noticia: {texto}")
        print(f"Autor: {autor}")
        print(f"URL: {link}")
        print("-" * 50)

        # Insertamos los datos en la base de datos
        cursor.execute(
            "INSERT INTO extracciones (titulo, descripcion, noticia, autor, fecha, url) VALUES (%s, %s, %s, %s, %s, %s)",
            (titulo, descripcion, texto, autor, fecha, link)
        )
        conn.commit()

    except (TimeoutException, NoSuchElementException, WebDriverException) as e:
        print(f"Hubo un error durante la extracción: {e}")
        cursor.execute("INSERT INTO extracciones (url) VALUES (%s)", (link,))
        conn.commit()
        print("No pudimos extraer la noticia. Pasamos a la siguiente.")

    finally:
        # Cerramos la pestaña actual y volvemos a la ventana principal
        if len(driver.window_handles) > 1:
            driver.switch_to.window(driver.window_handles[-1])
            driver.close()  # Cerramos la pestaña de la noticia
        driver.switch_to.window(original_window)  # Regresamos a la ventana principal
        print("Hemos cerrado la pestaña de la noticia y regresado a la ventana principal.")

        sleep(5)


def extraer_noticia_con_timeout(driver, link, cursor, conn, timeout=45):
    """Extraemos la noticia con un tiempo límite para evitar bloqueos."""
    thread = threading.Thread(target=extraer_noticia, args=(driver, link, cursor, conn))
    thread.start()

    # Esperamos a que el hilo termine dentro del tiempo límite
    thread.join(timeout)

    # Si el hilo sigue activo después del tiempo límite
    if thread.is_alive():
        print("Se agotó el tiempo para extraer la noticia. Pasamos a la siguiente.")

        # Intentamos cerrar la pestaña de la noticia si está abierta
        if len(driver.window_handles) > 1:
            driver.switch_to.window(driver.window_handles[-1])
            driver.close()

        # Regresamos a la ventana principal
        driver.switch_to.window(driver.window_handles[0])
        cursor.execute("INSERT INTO extracciones (url) VALUES (%s)", (link,))
        conn.commit()
        sleep(2)

        print("Hemos regresado a la ventana principal después del timeout.")
    else:
        print(f"La extracción de la noticia {link} se completó correctamente dentro del tiempo establecido.")


def check_url_exists(cursor, url):
    """Verificamos si la URL ya existe en la base de datos."""
    query = "SELECT 1 FROM extracciones WHERE url = %s LIMIT 1"
    cursor.execute(query, (url,))
    return cursor.fetchone() is not None


# Función para cerrar popups
def cerrar_popup(driver):
    """Intentamos cerrar cualquier popup que aparezca."""
    try:
        # Esperamos un momento para que el popup aparezca
        time.sleep(3)
        popup = WebDriverWait(driver, 10).until(
            EC.presence_of_element_located((By.CSS_SELECTOR, 'div#webpushBanner-eluniversal'))
        )
        boton_no_gracias = popup.find_element(By.XPATH, '//*[@id="webpushBanner-eluniversal"]/div/div/div[4]/button')
        if boton_no_gracias.is_displayed():
            boton_no_gracias.click()
            print("Hemos cerrado el popup 'No, gracias' exitosamente.")
    except Exception as e:
        print(f"No pudimos cerrar el popup 'No, gracias': {e}")


try:
    # Conectamos a la base de datos
    conn = mysql.connector.connect(**db_config)
    cursor = conn.cursor()

    # --- Verificamos la existencia de la tabla 'extracciones' ---
    # Si la tabla no existe, la creamos
    cursor.execute("""
        CREATE TABLE IF NOT EXISTS extracciones (
            id INT AUTO_INCREMENT PRIMARY KEY,
            titulo TEXT,
            descripcion TEXT,
            noticia TEXT,
            autor TEXT,
            fecha TEXT,
            url VARCHAR(255)
        );
    """)
    conn.commit()
    print("Hemos verificado la existencia de la tabla 'extracciones'.")

    # Navegamos a la página principal de El Universal
    driver.get('https://www.eluniversal.com.mx/')
    sleep(10)
    sleep(10)

    cerrar_popup(driver)
    cerrar_mensaje_cookies(driver)

    # Intentamos buscar y hacer clic en el botón para iniciar sesión
    try:
        sleep(4)
        sleep(4)
        cerrar_popup(driver)
        sleep(5)
        cerrar_popup(driver)
        boton_login = driver.find_element(By.XPATH, '//input[contains(@value,"Iniciar Sesión")]')
        boton_login.click()
        sleep(random.uniform(1, 3))
    except Exception as e:
        print("Hubo un error al intentar encontrar el botón de login:", e)
        driver.quit()
        exit()

    try:
        sleep(4)
        WebDriverWait(driver, 10).until(
            EC.frame_to_be_available_and_switch_to_it((By.XPATH, '//iframe[contains(@src, "login")]')))
    except Exception as e:
        print("Hubo un error al intentar cambiar al frame de login:", e)
        driver.quit()
        exit()

    try:
        sleep(4)
        campo_email = WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH,
                                                                                      '/html/body/app-main/app-widget/screen-layout/main/current-screen/div/screen-login/div[4]/p[1]/input')))
        sleep(3)
        campo_email.click()
        campo_email.send_keys('alexx_hern@outlook.com')
        sleep(random.uniform(1, 3))
    except Exception as e:
        print("Hubo un error al intentar ingresar el correo electrónico:", e)
        driver.quit()
        exit()

    try:
        campo_password = WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH,
                                                                                         '/html/body/app-main/app-widget/screen-layout/main/current-screen/div/screen-login/div[4]/p[2]/input')))
        campo_password.click()
        campo_password.send_keys('Soccer.8a')
        sleep(random.uniform(1, 3))
    except Exception as e:
        print("Hubo un error al intentar ingresar la contraseña:", e)
        driver.quit()
        exit()

    try:
        boton_submit = WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.XPATH,
                                                                                       '/html/body/app-main/app-widget/screen-layout/main/current-screen/div/screen-login/div[4]/p[3]/button')))
        boton_submit.click()
        sleep(random.uniform(1, 3))
    except Exception as e:
        print("Hubo un error al intentar hacer clic en el botón de login:", e)
        driver.quit()
        exit()

    driver.switch_to.default_content()
    sleep(random.uniform(3, 5))
    sleep(10)
    cerrar_mensaje_cookies(driver)

    cerrar_popup(driver)

    # Intentamos realizar la búsqueda de "secuestro" hasta 2 veces
    for intento in range(2):
        try:
            sleep(random.uniform(3, 5))
            cerrar_popup(driver)
            sleep(random.uniform(3, 5))
            cerrar_mensaje_cookies(driver)
            campo_busqueda = driver.find_element(By.XPATH, '//input[contains(@class,"buscadoreventod ml-2 py-2")]')
            campo_busqueda.click()
            sleep(random.uniform(5, 6))
            campo_busqueda.clear()
            campo_busqueda.send_keys('secuestro')
            sleep(random.uniform(5, 6))
            campo_busqueda.send_keys(Keys.RETURN)
            sleep(random.uniform(5, 6))
            sleep(10)

            WebDriverWait(driver, 10).until(EC.url_contains('query=secuestro'))
            sleep(random.uniform(5, 6))

            if 'query=secuestro' in driver.current_url:
                print(f"Hemos realizado la búsqueda correctamente en el intento {intento + 1}")
                break
        except Exception as e:
            print(f"Hubo un error al intentar realizar la búsqueda en el intento {intento + 1}:", e)
            if intento == 1:
                driver.quit()
                exit()

    contador_noticias = 0
    max_noticias = 15000
    sleep(10)
    sleep(10)
    pagina = 1  # Contador de páginas

    while contador_noticias < max_noticias:
        try:
            sleep(10)
            noticias = driver.find_elements(By.XPATH, '//*[@id="resultdata"]/div[@class="queryly_item_row"]/a')
            links_noticias = [tag_a.get_attribute("href") for tag_a in noticias if tag_a.get_attribute("href")]
            if pagina > 1:
                urls_nuevas = [url for url in links_noticias if not check_url_exists(cursor, url)]

                if not urls_nuevas:
                    print("Todas las URLs en esta página ya están en la base de datos. Pasamos a la siguiente.")
                    try:
                        sleep(random.uniform(1, 3))
                        try:
                            sleep(3)
                            boton_siguiente = driver.find_element(By.XPATH,
                                                                  '//a[@class="next_btn" and contains(text(),"Siguiente")]')
                        except:
                            boton_siguiente = driver.find_element(By.XPATH,
                                                                  '/html/body/div[1]/div[1]/div[2]/div[1]/div/div/div[2]/a[1]')
                        # Si tienes una función para desplazar al elemento, puedes descomentar la siguiente línea
                        # desplazar_a_elemento(driver, boton_siguiente)
                        boton_siguiente.click()
                        pagina += 1
                        sleep(random.uniform(1, 3))
                        continue
                    except Exception as e:
                        print("Hubo un error al intentar hacer clic en el botón siguiente:", e)
                        sleep(10)
                        try:
                            driver.get('https://www.eluniversal.com.mx/buscador/?query=secuestro')
                            sleep(3)
                            continue
                        except Exception as e:
                            print("No pudimos hacer clic en el botón 'Siguiente' después de esperar:", e)
                            break
            for link in links_noticias:
                if contador_noticias >= max_noticias:
                    break
                if check_url_exists(cursor, link):
                    print(f"La URL {link} ya está en la base de datos")
                    continue

                # Llamamos a la función para extraer la noticia con timeout
                extraer_noticia_con_timeout(driver, link, cursor, conn, timeout=60)
                contador_noticias += 1

        except Exception as e:
            print(f"Hubo un error durante la obtención de noticias: {e}")
            break

        # Intentamos ir a la siguiente página
        try:
            sleep(10)
            sleep(10)
            sleep(random.uniform(1, 3))  # Esperamos un momento para que la página cargue completamente
            try:
                boton_siguiente = driver.find_element(By.XPATH,
                                                      '//a[@class="next_btn" and contains(text(),"Siguiente")]')
            except:
                boton_siguiente = driver.find_element(By.XPATH,
                                                      '/html/body/div[1]/div[1]/div[2]/div[1]/div/div/div[2]/a[1]')
            # Si tienes una función para desplazar al elemento, puedes descomentar la siguiente línea
            # desplazar_a_elemento(driver, boton_siguiente)
            boton_siguiente.click()
            pagina += 1  # Incrementamos el contador de páginas
            sleep(random.uniform(1, 3))
            sleep(10)
        except Exception as e:
            print("Hubo un error al intentar hacer clic en el botón siguiente:", e)
            sleep(10)  # Esperamos 10 segundos para dar tiempo a que desaparezca cualquier anuncio
            try:
                driver.get('https://www.eluniversal.com.mx/buscador/?query=secuestro')
                sleep(3)
                sleep(10)
            except Exception as e:
                print("No pudimos hacer clic en el botón 'Siguiente' después de esperar:", e)
                break

finally:
    # Cerramos el driver y la conexión a la base de datos
    driver.quit()
    cursor.close()
    conn.close()
