import pymysql
import pandas as pd
from sqlalchemy import create_engine
import re
import spacy
import requests
import time
import unicodedata
from spacy.matcher import Matcher

# Cargamos el modelo spaCy para español
nlp = spacy.load('es_core_news_lg')

usuario_geonames = "bryanhernandez"

PALABRAS_IRRELEVANTES = [
    "El", "Los", "La", "Las", "Un", "Una", "De", "Del", "En", "Sin", "Con", "No", "Ven", "Al",
    "Centro", "Ciudad", "Norte", "Sur", "Este", "Oeste", "Ortega", "Felipe", "Cruz", "San", "General", "Todo",
    "Lo", "Nacional", "Por", "Durante", "Anaya", "Fuentes", "Instituto", "Han", "He", "Has", "Tu", "Progreso",
    "Internacional", "Fue", "Ocho", "Manuel", "Eduardo", "Como", "Gabriel", "Pero", "Para", "Rafael", "Juan", "Luis", "Tres",
    "Alto", "Uno", "Dos", "Tres", "Cuatro", "Cinco", "Carlos", "Gustavo", "Genaro", "Francisco", "Miguel", "Estado", "Jorge",
    "Nacional", "Casas", "Mata", "Santa", "China", "Agua", "Nuevo", "Valle", "Castillo", "Camargo", "Guadalupe", "Santiago", "Tierra",
    "Benito", "Nuevo", "Pedro", "Isidro", "José", "María", "Vicente", "Nicolas", "Emiliano", "Pueblo", "Casa", "Santa", "Padilla",
    "Marcos", "Soto", "Benito", "Ruiz", "Salvador", "Reforma", "Carrillo", "Martinez", "Gonzalez", "Reyes", "Solidaridad"
]
PALABRAS_IRRELEVANTES_LOWER = set(p.lower() for p in PALABRAS_IRRELEVANTES)

# Palabras clavel base y verbos clave para analizar relevancia
PALABRAS_CLAVE_BASE = [
    "secuestro", "hecho", "incidente", "caso", "ubicado", "encontrado", "rescatado"
]
VERBOS_CLAVE = [
    "ocurrir", "suceder", "realizar", "encontrar", "rescatar"
]

# abreviaturas de estados en México
ABREVIATURAS_ESTADOS = {
    "Ags.": "Aguascalientes",
    "BC.": "Baja California",
    "BCS.": "Baja California Sur",
    "Camp.": "Campeche",
    "Chis.": "Chiapas",
    "Chih.": "Chihuahua",
    "CDMX.": "Ciudad de México",
    "Coah.": "Coahuila",
    "Col.": "Colima",
    "Dgo.": "Durango",
    "Edomex.": "Estado de México",
    "Gto.": "Guanajuato",
    "Gro.": "Guerrero",
    "Hgo.": "Hidalgo",
    "Jal.": "Jalisco",
    "Mich.": "Michoacán",
    "Mor.": "Morelos",
    "Nay.": "Nayarit",
    "NL.": "Nuevo León",
    "Oax.": "Oaxaca",
    "Pue.": "Puebla",
    "Qro.": "Querétaro",
    "QR.": "Quintana Roo",
    "SLP.": "San Luis Potosí",
    "Sin.": "Sinaloa",
    "Son.": "Sonora",
    "Tab.": "Tabasco",
    "Tamps.": "Tamaulipas",
    "Tlax.": "Tlaxcala",
    "Ver.": "Veracruz",
    "Yuc.": "Yucatán",
    "Zac.": "Zacatecas"
}

# alias de lugares comunes
ALIAS_LUGARES = {
    "Tuxtla": "Tuxtla Gutiérrez",
    "Distrito Federal": "Ciudad de México",
    "Victoria": "Ciudad Victoria",
    "Izcalli": "Cuautitlán Izcalli"
}

# Lista de patrones para detectar métodos de captura
PATTERN_METHODS = {
    "Captura_Fuerza": [
        [{"LEMMA": {"IN": ["golpear", "forzar", "someter", "empujar", "agarrar"]}}],
        [{"TEXT": {"REGEX": "(golpeado|forzado|sometido|empujado|agarrado)"}}],
        [{"TEXT": {"REGEX": "a punta de pistola|con violencia|bajo amenazas"}}],
    ],
    "Captura_Emboscada": [
        [{"LEMMA": {"IN": ["emboscar", "interceptar", "rodear", "bloquear"]}}],
        [{"TEXT": {"REGEX": "en una emboscada|interceptaron su vehículo"}}],
        [{"TEXT": {"REGEX": "(emboscado|interceptado) en"}}],
    ],
    "Captura_Intimidacion": [
        [{"LEMMA": {"IN": ["amenazar", "intimidar", "coaccionar", "chantajear"]}}],
        [{"TEXT": {"REGEX": "(amenazado|intimidado) con"}}],
        [{"TEXT": {"REGEX": "amenazas de muerte|amenazándolo con"}}],
    ],
    "Captura_Tecnologica": [
        [
            {"TEXT": {"REGEX": "contactó|contactaron|engañado|engañada|citó|citaron"}},
            {"OP": "*"},
            {"TEXT": {"REGEX": "facebook|twitter|redes sociales|internet|aplicación móvil|app de citas"}}
        ],
        [
            {"LEMMA": {"IN": ["conocer", "interactuar"]}},
            {"TEXT": {"REGEX": "en línea|por internet|por redes sociales"}}
        ],
    ],
    "Captura_Confianza": [
        [{"TEXT": {"REGEX": "amigo|amiga|familiar|conocido|cercano"}}],
        [{"TEXT": {"REGEX": "persona de confianza|relación cercana"}}],
    ],
    "Captura_Autoridad": [
        [
            {"LEMMA": {"IN": ["policía", "policías", "agente", "agentes", "militar", "militares", "ejército", "autoridad", "autoridades"]}},
            {"OP": "+"},
            {"LEMMA": {"IN": ["secuestrar", "privar", "detener"]}, "OP": "+"}
        ],
    ],
    "Captura_Transporte": [
        [{"TEXT": {"REGEX": "autobús|camioneta|vehículo interceptado|taxi|transporte público"}}],
        [{"TEXT": {"REGEX": "camino a su destino|en tránsito|en ruta"}}],
    ],
    "Captura_Complicidad": [
        [{"TEXT": {"REGEX": "empleado|empleada|colaborador|compañero"}}],
        [{"TEXT": {"REGEX": "complicidad|alguien del entorno laboral"}}],
    ],
    "Captura_Cartel": [
        [{"TEXT": {"REGEX": "cártel|cartel|banda|grupo criminal|La Familia|Los Zetas"}}],
        [{"TEXT": {"REGEX": "vinculado a cartel|como represalia"}}],
    ],
    "Suplantacion_Identidad": [
        [{"LEMMA": {"IN": ["hacerse", "suplantar", "pretender", "imitar", "aparentar", "fingir", "simular"]}},
         {"OP": "*"},
         {"LEMMA": {"IN": ["policía", "agente", "militar", "autoridad", "funcionario"]}}]
    ],
    "Captura_Casa": [
        [{"TEXT": {"REGEX": "en su (propia )?casa|en su (propio )?domicilio|cerca de su hogar|afuera de su casa|entrando a su casa|saliendo de su casa"}}],
        [{"LEMMA": {"IN": ["hogar", "casa", "domicilio"]}}],
        [{"TEXT": {"REGEX": "propiedad"}}],
    ],
}

def obtener_conexion_pymysql():
    """Intentamos establecer una conexión con la base de datos utilizando pymysql."""
    try:
        conexion = pymysql.connect(
            host='localhost',
            user='root',
            password='Soccer.8a',  # **Reemplaza con tu contraseña**
            database='noticias',
            charset='utf8mb4',
            cursorclass=pymysql.cursors.DictCursor
        )
        return conexion
    except pymysql.MySQLError as e:
        print(f"Error al conectar a la base de datos: {e}")
        return None

def obtener_conexion_sqlalchemy():
    """Intentamos establecer una conexión con la base de datos utilizando SQLAlchemy."""
    try:
        engine = create_engine('mysql+pymysql://root:Soccer.8a@localhost:3306/noticias')
        return engine
    except Exception as e:
        print(f"Error al conectar con SQLAlchemy: {e}")
        return None

def normalizar_texto(texto):
    """Normalizamos el texto eliminando acentos y convirtiendo a minúsculas."""
    texto = unicodedata.normalize('NFKD', texto).encode('ASCII', 'ignore').decode('ASCII')
    return texto.lower()


def asegurar_columna_noticia_corregida(conexion):
    """Nos aseguramos de que la columna 'noticia_corregida' exista y la creamos si no."""
    with conexion.cursor() as cursor:
        cursor.execute("SHOW COLUMNS FROM extracciones LIKE 'noticia_corregida';")
        resultado = cursor.fetchone()
        if not resultado:
            cursor.execute("ALTER TABLE extracciones ADD COLUMN noticia_corregida TEXT;")
            print("Hemos añadido el campo 'noticia_corregida' a la tabla 'extracciones'.")
    conexion.commit()

def limpiar_texto_noticia(texto):
    """Limpiamos el texto de la noticia utilizando expresiones regulares."""
    exreg_lee_tambien = re.compile(
        r'([Ll]ee también|[Ll]eer también|[Ll]ea también|[Ll]ee más|[Tt]ambién lee|[Tt]ambien lee).*?(\n|$)', re.IGNORECASE)
    exreg_foto = re.compile(r'Foto:.*?(\n|$)', re.IGNORECASE)
    exreg_dispositivo = re.compile(
        r',\s*desde tu dispositivo móvil entérate de las noticias más relevantes del día, artículos de opinión, entretenimiento, tendencias y más\..*?(\n|$)',
        re.IGNORECASE)
    exreg_ultima_parte = re.compile(
        r'(\*?\s*El Grupo de Diarios América \(GDA\), al cual pertenece EL UNIVERSAL.*|'
        r'Ahora puedes recibir notificaciones de BBC Mundo.*|'
        r'Recuerda que puedes recibir notificaciones de BBC Mundo.*|'
        r'Suscríbete aquí.*|'
        r'Recibe todos los viernes Hello Weekend.*|'
        r'Recuerda que puedes recibir notificaciones de BBC News Mundo.*|'
        r'Únete a nuestro canal.*|'
        r'Ahora puedes recibir notificaciones de BBC News Mundo.*|'
        r'¿Ya conoces nuestro canal de YouTube\? ¡Suscríbete!.*|'
        r'para recibir directo en tu correo nuestras newsletters sobre noticias del día, opinión, (planes para el fin de semana, )?Qatar 2022 y muchas opciones más\..*)',
        re.IGNORECASE | re.DOTALL)

    texto_limpio = re.sub(exreg_lee_tambien, '', texto)
    texto_limpio = re.sub(exreg_foto, '', texto_limpio)
    texto_limpio = re.sub(exreg_dispositivo, '', texto_limpio)
    texto_limpio = re.sub(exreg_ultima_parte, '', texto_limpio)

    return texto_limpio

def limpiar_noticias():
    """Limpiamos las noticias originales y actualizamos la columna 'noticia_corregida'."""
    conexion = obtener_conexion_pymysql()
    if not conexion:
        return
    try:
        asegurar_columna_noticia_corregida(conexion)
        with conexion.cursor() as cursor:
            consulta_seleccion = "SELECT id, noticia FROM extracciones"
            cursor.execute(consulta_seleccion)
            resultados = cursor.fetchall()

            ids_modificados = []
            for fila in resultados:
                id_noticia = fila['id']
                texto_noticia = fila['noticia']
                if texto_noticia is not None:
                    texto_noticia_limpio = limpiar_texto_noticia(texto_noticia)
                    if texto_noticia != texto_noticia_limpio:
                        ids_modificados.append(id_noticia)
                    consulta_actualizacion = "UPDATE extracciones SET noticia_corregida = %s WHERE id = %s"
                    cursor.execute(consulta_actualizacion, (texto_noticia_limpio, id_noticia))
            conexion.commit()

        if ids_modificados:
            print("Hemos procesado las noticias con los siguientes IDs:")
            print(ids_modificados)
        else:
            print("No se realizaron modificaciones en las noticias.")
    except pymysql.MySQLError as e:
        print(f"Hubo un error al limpiar las noticias: {e}")
    finally:
        conexion.close()

# DETECCIÓN DE SECUESTROS ---

def es_noticia_de_secuestro(texto_completo):
    """Determinamos si una noticia está relacionada con secuestros."""
    doc = nlp(texto_completo)
    es_secuestro = False
    justificacion = ""

    for ent in doc.ents:
        contexto = ent.sent.text
        if any(term in contexto.lower() for term in ['simulacro', 'película', 'serie', 'ficticio', 'ficción']):
            es_secuestro = False
            justificacion = f"Contexto no real detectado: '{contexto}'"
            break
        if ent.label_ in ['PER', 'ORG', 'MISC']:
            if any(verb in contexto for verb in ['retenido', 'privado', 'capturado', 'detenido', 'secuestrado']):
                es_secuestro = True
                justificacion = f"Se encontró contexto de posible secuestro: '{contexto}'"
                break
        if "víctima" in ent.text.lower() and any(action in ent.sent.text.lower() for action in ['retenida', 'privada de libertad']):
            es_secuestro = True
            justificacion = f"Víctima privada de libertad detectada: '{ent.sent.text}'"
            break

    # Si no se ha determinado aún, buscar términos clave en todo el texto
    if not es_secuestro and not justificacion:
        texto_normalizado = normalizar_texto(texto_completo)
        if any(term in texto_normalizado for term in PALABRAS_CLAVE_BASE):
            es_secuestro = True
            justificacion = "Se encontraron palabras clave relacionadas con secuestro en el texto."
        else:
            justificacion = "No se encontraron indicios de secuestro en el texto."

    return es_secuestro, justificacion

def obtener_noticias_secuestro():
    """Recuperamos las noticias que no contienen ciertos términos y son candidatas a ser secuestros."""
    conexion = obtener_conexion_pymysql()
    if not conexion:
        return []
    try:
        with conexion.cursor() as cursor:
            sql = """
            SELECT id, noticia_corregida
            FROM extracciones
            WHERE (titulo NOT LIKE '%El Mayo Zambada%' 
            AND descripcion NOT LIKE '%El Mayo Zambada%' 
            AND titulo NOT LIKE '%El Mayo%' 
            AND descripcion NOT LIKE '%El Mayo%' 
            AND titulo NOT LIKE '%Israel%' 
            AND descripcion NOT LIKE '%Israel%' 
            AND titulo NOT LIKE '%Gaza%' 
            AND descripcion NOT LIKE '%Gaza%' 
            AND titulo NOT LIKE '%Hamas%' 
            AND descripcion NOT LIKE '%Hamas%' 
            AND titulo NOT LIKE '%Netanyahu%' 
            AND descripcion NOT LIKE '%Netanyahu%'
            AND titulo NOT LIKE '%Chapo Guzmán%' 
            AND descripcion NOT LIKE '%Chapo Guzmán%' 
            AND titulo NOT LIKE '%Ovidio Guzmán%' 
            AND descripcion NOT LIKE '%Ovidio Guzmán%');
            """
            cursor.execute(sql)
            resultados = cursor.fetchall()
            return resultados
    except pymysql.MySQLError as e:
        print(f"Hubo un error al obtener noticias para secuestro: {e}")
        return []
    finally:
        conexion.close()

def procesar_noticias_secuestro():
    """Procesamos las noticias para determinar si están relacionadas con secuestros."""
    noticias = obtener_noticias_secuestro()
    if not noticias:
        print("No hay noticias para procesar en la detección de secuestros.")
        return

    conexion = obtener_conexion_pymysql()
    if not conexion:
        return

    try:
        with conexion.cursor() as cursor:
            # Verificar y agregar el campo 'relacion_spacy4' si no existe
            cursor.execute("SHOW COLUMNS FROM extracciones LIKE 'relacion_spacy4';")
            resultado = cursor.fetchone()
            if not resultado:
                cursor.execute("ALTER TABLE extracciones ADD COLUMN relacion_spacy4 VARCHAR(3) DEFAULT 'no';")
                print("Hemos añadido el campo 'relacion_spacy4' a la tabla 'extracciones'.")

            for noticia in noticias:
                id_noticia = noticia['id']
                texto_completo = noticia['noticia_corregida'] if noticia['noticia_corregida'] else ""
                relacionada_con_secuestro, justificacion = es_noticia_de_secuestro(texto_completo)
                if relacionada_con_secuestro:
                    cursor.execute("UPDATE extracciones SET relacion_spacy4 = 'sí' WHERE id = %s", (id_noticia,))
                    print(f"Noticia ID {id_noticia} relacionada con secuestro. Justificación: {justificacion}")
                else:
                    cursor.execute("UPDATE extracciones SET relacion_spacy4 = 'no' WHERE id = %s", (id_noticia,))
                    print(f"Noticia ID {id_noticia} NO relacionada con secuestro. Justificación: {justificacion}")
            conexion.commit()
    except pymysql.MySQLError as e:
        print(f"Hubo un error al procesar noticias de secuestro: {e}")
    finally:
        conexion.close()

# --- TERCERA PARTE: Extraemos las ubicaciones ---

def agregar_campos_geograficos():
    """Nos aseguramos de que las columnas geográficas existan y las creamos si no."""
    conexion = obtener_conexion_pymysql()
    if not conexion:
        return
    try:
        with conexion.cursor() as cursor:
            # Lista de columnas geográficas a verificar y agregar si no existen
            columnas_geograficas = {
                'pais': "VARCHAR(255)",
                'estado': "VARCHAR(255)",
                'municipio': "VARCHAR(255)",
                'ciudad': "VARCHAR(255)"
            }

            for campo, tipo in columnas_geograficas.items():
                cursor.execute(f"SHOW COLUMNS FROM extracciones LIKE '{campo}';")
                resultado = cursor.fetchone()
                if not resultado:
                    cursor.execute(f"ALTER TABLE extracciones ADD COLUMN {campo} {tipo};")
                    print(f"Hemos añadido el campo '{campo}' a la tabla 'extracciones'.")
        conexion.commit()
    except pymysql.MySQLError as e:
        print(f"Hubo un error al agregar campos geográficos: {e}")
        conexion.rollback()
    finally:
        conexion.close()

def generar_conjugaciones(verbos_clave):
    """Generamos conjugaciones de los verbos clave para el análisis."""
    conjugaciones = set()
    for verbo in verbos_clave:
        doc = nlp(verbo)
        for token in doc:
            if token.pos_ == "VERB":
                conjugaciones.add(token.lemma_)
                conjugaciones.add(token.text.lower())
                conjugaciones.update([
                    f"{token.lemma_}á",
                    f"{token.lemma_}ía",
                    f"{token.lemma_}ó",
                    f"ha {token.lemma_}",
                    f"había {token.lemma_}",
                    f"habrá {token.lemma_}",
                    f"haya {token.lemma_}",
                    f"hubiera {token.lemma_}"
                ])
    return conjugaciones

def validar_relacion_hechos(texto, lugares):
    """Validamos la relación de hechos con lugares mediante relevancia semántica."""
    doc = nlp(texto)
    relevancia = {}
    conjugaciones = generar_conjugaciones(VERBOS_CLAVE)
    palabras_clave = set(PALABRAS_CLAVE_BASE).union(conjugaciones)
    for lugar in lugares:
        relevancia[lugar] = 0
        for token in doc:
            if lugar.lower() in token.text.lower():
                for palabra in palabras_clave:
                    # Verificamos si la palabra clave está en el árbol sintáctico del token
                    if palabra in [w.text.lower() for w in token.head.subtree]:
                        relevancia[lugar] += 1
    lugar_mas_relevante = max(relevancia, key=relevancia.get) if relevancia else None
    return lugar_mas_relevante

def extraer_primer_lugar(texto):
    """Extraemos el primer lugar mencionado en el texto utilizando expresiones regulares."""
    regex_inicio = r"^(?P<lugar>[A-ZÁÉÍÓÚÑa-záéíóúñ\s]+),?\s?(?P<estado_abrev>[A-Z][a-z]+\.)\s?[-—\.]"
    coincidencia = re.match(regex_inicio, texto)
    if coincidencia:
        lugar = coincidencia.group("lugar").strip()
        estado_abrev = coincidencia.group("estado_abrev").strip()
        estado_completo = ABREVIATURAS_ESTADOS.get(estado_abrev, estado_abrev)
        lugar_completo = f"{lugar}, {estado_completo}"
        return lugar_completo
    return None

def extraer_lugares_regex(texto):
    """Extraemos lugares mencionados en el texto utilizando expresiones regulares."""
    regex = r"(?:^|\.\s|\-\s|\b)([A-Z][a-z]+(?: [A-Z][a-z]+)*)[\.\-]?\b"
    lugares = re.findall(regex, texto)
    return lugares

def conectar_bd_local():
    """Intentamos conectar a la base de datos local de municipios/estados."""
    try:
        connection = pymysql.connect(
            host='localhost',
            user='root',
            password='Soccer.8a',
            db='noticias',
            charset='utf8mb4',
            cursorclass=pymysql.cursors.DictCursor
        )
        return connection
    except pymysql.MySQLError as e:
        print(f"Hubo un error al conectar con la base de datos local: {e}")
        return None

def validar_lugar_bd_local(lugar):
    """Validamos si un lugar existe en la base de datos local de municipios/estados."""
    connection = conectar_bd_local()
    if not connection:
        return None, None, None
    try:
        cursor = connection.cursor()
        if lugar in ALIAS_LUGARES:
            lugar = ALIAS_LUGARES[lugar]

        # Verificamos el estado exacto
        sql_estado = "SELECT nombre, 'México' AS pais FROM estados WHERE nombre = %s"
        cursor.execute(sql_estado, (lugar,))
        resultado_estado = cursor.fetchone()
        if resultado_estado:
            estado = resultado_estado['nombre']
            pais = resultado_estado['pais']
            return pais, estado, None

        # Verificamos el municipio exacto
        sql_municipio = """
        SELECT municipios.nombre AS municipio, estados.nombre AS estado, 'México' AS pais
        FROM municipios
        INNER JOIN estados ON municipios.estado = estados.id
        WHERE municipios.nombre = %s
        """
        cursor.execute(sql_municipio, (lugar,))
        resultado_municipio = cursor.fetchone()
        if resultado_municipio:
            municipio = resultado_municipio['municipio']
            estado = resultado_municipio['estado']
            pais = resultado_municipio['pais']
            return pais, estado, municipio

        # Coincidencias parciales para municipios
        if " " not in lugar.strip():
            sql_municipio_parcial = """
            SELECT municipios.nombre AS municipio, estados.nombre AS estado, 'México' AS pais
            FROM municipios
            INNER JOIN estados ON municipios.estado = estados.id
            WHERE municipios.nombre LIKE %s
            """
            cursor.execute(sql_municipio_parcial, (f"{lugar} %",))
            resultado_municipio_parcial = cursor.fetchone()
            if resultado_municipio_parcial:
                municipio = resultado_municipio_parcial['municipio']
                estado = resultado_municipio_parcial['estado']
                pais = resultado_municipio_parcial['pais']
                return pais, estado, municipio

            # Coincidencias parciales para estados
            sql_estado_parcial = "SELECT nombre, 'México' AS pais FROM estados WHERE nombre LIKE %s"
            cursor.execute(sql_estado_parcial, (f"{lugar} %",))
            resultado_estado_parcial = cursor.fetchone()
            if resultado_estado_parcial:
                estado = resultado_estado_parcial['nombre']
                pais = resultado_estado_parcial['pais']
                return pais, estado, None

        return None, None, None
    except pymysql.MySQLError as e:
        print(f"Hubo un error al validar lugar en la BD local: {e}")
        return None, None, None
    finally:
        connection.close()

def validar_lugar_via_geonames(lugar, usuario):
    """Validamos un lugar utilizando la API de GeoNames."""
    if lugar.lower() in PALABRAS_IRRELEVANTES_LOWER or len(lugar) < 4:
        print(f"Lugar '{lugar}' ignorado por estar en la lista de palabras irrelevantes o ser demasiado corto.")
        return None, None, None
    url = f"http://api.geonames.org/searchJSON?q={lugar}&maxRows=1&username={usuario}&countryBias=MX&continentCode=SA"
    try:
        response = requests.get(url)
        if response.status_code == 200:
            data = response.json()
            if 'geonames' in data and len(data['geonames']) > 0:
                lugar_info = data['geonames'][0]
                pais = lugar_info.get('countryName', None)
                admin1 = lugar_info.get('adminName1', None)
                municipio = lugar_info.get('name', None)
                if pais and admin1:
                    if pais == "México":
                        return pais, admin1, municipio
                    else:
                        return pais, admin1, None
                if pais:
                    return pais, None, None
                return None, None, None
            else:
                print(f"No se encontraron resultados para '{lugar}' en GeoNames.")
                return None, None, None
        else:
            print(f"Hubo un error en la solicitud a GeoNames: {response.status_code}")
            return None, None, None
    except Exception as e:
        print(f"Hubo un error al conectar con GeoNames: {e}")
        return None, None, None

def extraer_lugares(texto):
    """Extraemos información geográfica del texto de la noticia."""
    # Paso 1: Extraemos el primer lugar del texto
    lugar_inicio = extraer_primer_lugar(texto)
    if lugar_inicio:
        print(f"Primer lugar detectado: {lugar_inicio}")
        pais_local, estado_local, municipio_local = validar_lugar_bd_local(lugar_inicio)
        if pais_local and estado_local:
            print(f"Lugar al inicio validado localmente: {lugar_inicio} -> {estado_local}, {pais_local}")
            return pais_local, estado_local, municipio_local, None, [f"Lugar inicial '{lugar_inicio}' validado"]

    # Paso 2: Extraemos lugares usando regex
    lugares_regex = extraer_lugares_regex(texto)
    print(f"Paso 2 - Lugares extraídos por regex: {lugares_regex}")

    pais = None
    estado = None
    municipio = None
    justificacion = []
    lugares_validados = []

    for lugar in lugares_regex:
        if lugar.lower() in PALABRAS_IRRELEVANTES_LOWER or len(lugar) < 4:
            print(f"Ignorando lugar irrelevante o demasiado corto: {lugar}")
            continue
        pais_local, estado_local, municipio_local = validar_lugar_bd_local(lugar)
        if pais_local and estado_local:
            pais = pais_local
            estado = estado_local
            municipio = municipio_local
            lugares_validados.append((pais, estado, municipio))
            justificacion.append(f"'{lugar}' validado localmente: {estado}, {pais}")
            print("Lugar completo validado. Detenemos la búsqueda adicional.")
            break  # Detenemos la búsqueda adicional al encontrar un lugar válido

    # Paso 3: Análisis semántico si hay múltiples lugares validados
    if len(lugares_validados) > 1:
        lugares_nombres = [f"{m or ''}, {e}" for _, e, m in lugares_validados]
        lugar_relevante = validar_relacion_hechos(texto, lugares_nombres)
        justificacion.append(f"Lugar más relevante según análisis semántico: {lugar_relevante}")
        for pais_val, estado_val, municipio_val in lugares_validados:
            if lugar_relevante and lugar_relevante in f"{municipio_val or ''}, {estado_val}":
                pais, estado, municipio = pais_val, estado_val, municipio_val
                break

    # Paso 4: Validamos lugares via GeoNames si no se encontró información suficiente
    if not pais or not estado:
        for lugar in lugares_regex:
            if lugar.lower() in PALABRAS_IRRELEVANTES_LOWER or len(lugar) < 4:
                print(f"Ignorando lugar irrelevante o demasiado corto: {lugar}")
                continue
            pais_geo, estado_geo, municipio_geo = validar_lugar_via_geonames(lugar, usuario_geonames)
            if pais_geo and estado_geo:
                pais = pais_geo
                estado = estado_geo
                municipio = municipio_geo
                justificacion.append(f"'{lugar}' clasificado en GeoNames: {estado_geo}, {pais_geo}")
                break

    print(f"Resultado Final - País: {pais}, Estado: {estado}, Municipio: {municipio}")
    print(f"Justificación Final de lugares: {justificacion}")

    return pais, estado, municipio, None, justificacion

def actualizar_base_datos_geograficos(pais, estado, municipio, ciudad, noticia_id):
    """Actualizamos los campos geográficos en la base de datos para una noticia específica."""
    conexion = obtener_conexion_pymysql()
    if not conexion:
        return
    try:
        with conexion.cursor() as cursor:
            sql = "UPDATE extracciones SET pais=%s, estado=%s, municipio=%s, ciudad=%s WHERE id=%s"
            cursor.execute(sql, (pais, estado, municipio, ciudad, noticia_id))
        conexion.commit()
        print(f"Noticia {noticia_id} actualizada con País: {pais}, Estado: {estado}, Municipio: {municipio}, Ciudad: {ciudad}")
    except pymysql.MySQLError as e:
        print(f"Hubo un error al actualizar la base de datos geográfica: {e}")
        conexion.rollback()
    finally:
        conexion.close()

def procesar_noticias_lugares():
    """Procesamos las noticias relacionadas con secuestros para extraer información geográfica."""
    conexion = obtener_conexion_pymysql()
    if not conexion:
        return
    try:
        with conexion.cursor() as cursor:
            # Seleccionamos noticias con relacion_spacy4='sí'
            sql = "SELECT id, noticia_corregida, pais, estado, municipio, ciudad FROM extracciones WHERE relacion_spacy4='sí'"
            cursor.execute(sql)
            noticias = cursor.fetchall()

        for noticia in noticias:
            noticia_id = noticia['id']
            texto_noticia = noticia['noticia_corregida'] or ""
            pais_actual = noticia['pais']
            estado_actual = noticia['estado']
            municipio_actual = noticia['municipio']
            ciudad_actual = noticia['ciudad']

            if pais_actual or estado_actual or municipio_actual or ciudad_actual:
                print(f"Saltamos el análisis para la noticia {noticia_id} (ya tiene campos geográficos llenos)")
                continue

            inicio = time.time()
            pais, estado, municipio, ciudad, justificacion = extraer_lugares(texto_noticia)
            fin = time.time()
            tiempo_procesamiento = fin - inicio

            print(f"\nNoticia ID: {noticia_id}")
            print(f"Justificación de lugares: {justificacion}")
            print(f"País extraído: {pais}")
            print(f"Estado extraído: {estado}")
            print(f"Municipio extraído: {municipio}")
            print(f"Ciudad extraída: {ciudad}")
            print(f"Tiempo de procesamiento: {tiempo_procesamiento:.2f} segundos")

            if pais or estado or municipio or ciudad:
                actualizar_base_datos_geograficos(pais, estado, municipio, ciudad, noticia_id)

    except pymysql.MySQLError as e:
        print(f"Hubo un error al procesar noticias geográficas: {e}")
    finally:
        conexion.close()

# --- CUARTA PARTE: DETECCIÓN DE MÉTODOS DE CAPTURA ---

def detectar_metodo_captura(texto):
    """Detectamos el método de captura utilizado en la noticia."""
    doc = nlp(texto.lower())
    matcher = Matcher(nlp.vocab)

    # Añadimos los patrones al matcher
    for method_name, patterns in PATTERN_METHODS.items():
        matcher.add(method_name, patterns)

    captor_methods = []
    lugar_methods = []
    captura_methods = []  # Lista para capturas
    suplantacion_detectada = False
    explicacion = {}  # Diccionario para almacenar explicaciones de cada método

    oraciones_ignoradas = set()
    palabras_clave_reporte = ["reportó", "denunció", "informó a la policía", "declaró a las autoridades"]
    palabras_clave_victima = ["policía fue secuestrado", "agente fue secuestrado", "militar fue secuestrado"]

    matches = matcher(doc)
    if not matches:
        print("No se encontraron coincidencias en el texto.")

    for match_id, start, end in matches:
        span = doc[start:end]
        oracion_completa = span.sent.text
        metodo = nlp.vocab.strings[match_id]

        # Verificamos si la referencia a la autoridad es un reporte o la autoridad es víctima
        if metodo == "Captura_Autoridad":
            # Si la oración contiene palabras que indican que es un reporte, la ignoramos
            if any(palabra in oracion_completa.lower() for palabra in palabras_clave_reporte):
                if oracion_completa not in oraciones_ignoradas:
                    oraciones_ignoradas.add(oracion_completa)
                continue  # No clasificamos como Captura_Autoridad

            # Si la autoridad es víctima, la ignoramos
            if any(palabra in oracion_completa.lower() for palabra in palabras_clave_victima):
                continue  # No clasificamos como Captura_Autoridad

            # **Nueva Verificación**: Comprobamos que estén perpetrando un secuestro o privación de libertad
            palabras_clave_acciones = ["secuestrar", "secuestro", "privación de libertad", "privar de libertad", "raptar"]
            if not any(palabra in oracion_completa.lower() for palabra in palabras_clave_acciones):
                continue

            # Si llegamos aquí, la autoridad es el perpetrador de secuestro o privación de libertad
            if "autoridad" not in captor_methods:
                captor_methods.append("autoridad")
                explicacion[metodo] = f"Método de captor detectado: 'autoridad'. Contexto: '{oracion_completa}'"
            continue  # Pasamos a la siguiente coincidencia

        # Detectamos y registramos suplantación de identidad
        if metodo == "Suplantacion_Identidad":
            suplantacion_detectada = True
            if "suplantación de identidad" not in captor_methods:
                captor_methods.append("suplantación de identidad")
                explicacion["Suplantacion_Identidad"] = f"Se detectó suplantación de identidad: '{oracion_completa}'"
            continue  # Pasamos a la siguiente coincidencia

        # Procesamos métodos de captor
        if metodo in ["Captura_Confianza", "Captura_Cartel", "Captura_Complicidad"]:
            captor_name = metodo.split('_')[1].lower()  # Obtenemos 'confianza', 'cartel', 'complicidad'
            if captor_name not in captor_methods:
                captor_methods.append(captor_name)
                explicacion[metodo] = f"Método de captor detectado: '{captor_name}'. Contexto: '{oracion_completa}'"

        # Procesamos métodos de lugar
        if metodo in ["Captura_Transporte", "Captura_Casa"]:
            lugar_name = metodo.split('_')[1].lower()  # Obtenemos 'transporte', 'casa'
            if lugar_name not in lugar_methods:
                lugar_methods.append(lugar_name)
                explicacion[metodo] = f"Lugar detectado: '{lugar_name}'. Contexto: '{oracion_completa}'"

        # Procesamos métodos de captura
        if metodo in ["Captura_Fuerza", "Captura_Emboscada", "Captura_Intimidacion", "Captura_Tecnologica"]:
            captura_name = metodo.split('_')[1].lower()  # Obtenemos 'fuerza', 'emboscada', etc.
            if captura_name not in captura_methods:
                captura_methods.append(captura_name)
                explicacion[metodo] = f"Método de captura detectado: '{captura_name}'. Contexto: '{oracion_completa}'"

    # Si no detectamos captor, asignamos 'persona común'
    if not captor_methods:
        captor_methods.append("persona común")

    # Si no detectamos lugar, asignamos 'vía pública'
    if not lugar_methods:
        lugar_methods.append("vía pública")

    # Asignamos 'no especifico' si no detectamos método de captura
    if not captura_methods:
        captura = "no especifico"
    else:
        captura = captura_methods[0]  # Solo el primero

    # Convertimos listas a cadenas
    captor = captor_methods[0]  # Solo el primero
    lugar = lugar_methods[0]    # Solo el primero

    # Preparamos explicaciones
    explicaciones_final = list(explicacion.values())

    return captor, lugar, captura, explicaciones_final

def verificar_y_crear_campos_captura():
    """Nos aseguramos de que las columnas 'captor', 'lugar', 'captura' existan y las creamos si no."""
    conexion = obtener_conexion_pymysql()
    if not conexion:
        return
    try:
        with conexion.cursor() as cursor:
            # Obtenemos las columnas existentes en la tabla
            cursor.execute("SHOW COLUMNS FROM extracciones")
            columnas = [row['Field'] for row in cursor.fetchall()]

            # Lista de nuevos campos que queremos agregar
            nuevos_campos = {
                'captor': "VARCHAR(255) DEFAULT NULL",
                'lugar': "VARCHAR(255) DEFAULT NULL",
                'captura': "VARCHAR(255) DEFAULT NULL"
            }

            # Iteramos sobre los nuevos campos y los agregamos si no existen
            for campo, definicion in nuevos_campos.items():
                if campo not in columnas:
                    sql_alter = f"ALTER TABLE extracciones ADD COLUMN {campo} {definicion};"
                    cursor.execute(sql_alter)
                    print(f"Hemos añadido el campo '{campo}' a la tabla 'extracciones'.")
        conexion.commit()
    except pymysql.MySQLError as e:
        print(f"Hubo un error al verificar o agregar campos de captura: {e}")
        conexion.rollback()
    finally:
        conexion.close()

def obtener_noticias_captura():
    """Recuperamos las noticias que están relacionadas con secuestros para analizar métodos de captura."""
    conexion = obtener_conexion_pymysql()
    if not conexion:
        return []
    try:
        with conexion.cursor() as cursor:
            sql = "SELECT id, noticia_corregida FROM extracciones WHERE relacion_spacy4 = 'sí'"
            cursor.execute(sql)
            resultados = cursor.fetchall()
            return resultados
    except pymysql.MySQLError as e:
        print(f"Hubo un error al obtener noticias para captura: {e}")
        return []
    finally:
        conexion.close()

def guardar_resultados_en_extracciones_captura(noticia_id, captor, lugar, captura):
    """Guardamos los resultados de la detección de métodos de captura en la base de datos."""
    conexion = obtener_conexion_pymysql()
    if not conexion:
        return
    try:
        with conexion.cursor() as cursor:
            sql = """
            UPDATE extracciones
            SET captor = %s, lugar = %s, captura = %s
            WHERE id = %s
            """
            cursor.execute(sql, (captor, lugar, captura, noticia_id))
        conexion.commit()
        print(f"Hemos guardado los resultados exitosamente para la noticia ID {noticia_id}")
    except pymysql.MySQLError as e:
        print(f"Hubo un error al guardar los resultados en 'extracciones': {e}")
        conexion.rollback()
    finally:
        conexion.close()

def procesar_noticias_captura():
    """Procesamos las noticias relacionadas con secuestros para detectar métodos de captura."""
    noticias = obtener_noticias_captura()
    if not noticias:
        print("No hay noticias para procesar en la detección de métodos de captura.")
        return

    conexion = obtener_conexion_pymysql()
    if not conexion:
        return

    try:
        with conexion.cursor() as cursor:
            for noticia in noticias:
                noticia_id = noticia['id']
                texto_noticia = noticia['noticia_corregida']
                print(f"\n--- Analizando noticia con ID: {noticia_id} ---")
                captor, lugar, captura, explicaciones = detectar_metodo_captura(texto_noticia)

                # Guardamos los resultados en la base de datos
                guardar_resultados_en_extracciones_captura(noticia_id, captor, lugar, captura)

                # Mostramos la explicación
                for exp in explicaciones:
                    print(f"- {exp}")

                print(f"Captor: {captor}")
                print(f"Lugar: {lugar}")
                print(f"Captura: {captura}")
    except pymysql.MySQLError as e:
        print(f"Hubo un error al procesar noticias de captura: {e}")
    finally:
        conexion.close()

# --- QUINTA PARTE: CLASIFICACIÓN DE LIBERACIÓN ---

def clasificar_liberacion(texto):
    """Clasificamos el tipo de liberación en una noticia."""
    doc = nlp(texto.lower())
    matcher = Matcher(nlp.vocab)

    # Patrones específicos para clasificar liberación
    patrones_liberacion_general = [
        [{"LEMMA": {"IN": ["liberar", "rescatar"]}}, {"OP": "+"}],
        [{"TEXT": {"REGEX": "liberado|liberaron|rescatado|rescatados|apareció sano y salvo|retornó a su hogar"}}]
    ]

    patrones_operativo = [
        [{"LEMMA": {"IN": ["operativo", "rescatar", "encontrar"]}}, {"LOWER": "policiaco", "OP": "?"}],
        [{"TEXT": {"REGEX": "fueron rescatados|fueron liberados"}}]
    ]

    patrones_autoridad = [
        [{"LEMMA": {"IN": ["elemento", "ejército", "autoridad"]}}, {"LOWER": "mexicano", "OP": "?"},
         {"LOWER": "liberar", "OP": "+"}]
    ]

    patrones_retorno = [
        [{"LEMMA": {"IN": ["retornar", "regresar", "volver"]}}, {"TEXT": {"REGEX": "a su hogar|sano y salvo"}}]
    ]

    patrones_negociacion = [
        [{"LEMMA": {"IN": ["negociar", "acordar"]}},
         {"LOWER": {"IN": ["liberación", "rescate", "retorno"]}, "OP": "?"}],
        [{"TEXT": {"REGEX": "negociación para la liberación|acuerdo de liberación|liberación por acuerdo"}}]
    ]

    # Añadimos los patrones al matcher
    matcher.add("LiberacionGeneral", patrones_liberacion_general)
    matcher.add("Operativo", patrones_operativo)
    matcher.add("Autoridad", patrones_autoridad)
    matcher.add("Retorno", patrones_retorno)
    matcher.add("Negociacion", patrones_negociacion)

    # Inicializamos variables de clasificación y justificación
    tipo_liberacion = "No clasificado"
    hubo_liberacion = False

    matches = matcher(doc)

    for match_id, start, end in matches:
        span = doc[start:end]
        tipo = nlp.vocab.strings[match_id]

        # Clasificación basada en tipo de patrón detectado
        if tipo == "LiberacionGeneral":
            tipo_liberacion = "Liberación general"
            hubo_liberacion = True
            break
        elif tipo == "Operativo":
            tipo_liberacion = "Liberación en operativo"
            hubo_liberacion = True
        elif tipo == "Autoridad":
            tipo_liberacion = "Liberación por autoridad"
            hubo_liberacion = True
        elif tipo == "Retorno":
            tipo_liberacion = "Retorno sin detalles"
            hubo_liberacion = True
        elif tipo == "Negociacion":
            tipo_liberacion = "Liberación por negociación"
            hubo_liberacion = True

    return hubo_liberacion, tipo_liberacion

def verificar_y_agregar_campos_liberacion():
    """Nos aseguramos de que las columnas 'liberacion' y 'tipo_liberacion' existan y las creamos si no."""
    conexion = obtener_conexion_pymysql()
    if not conexion:
        return
    try:
        with conexion.cursor() as cursor:
            # Verificamos si los campos existen en la tabla
            cursor.execute("SHOW COLUMNS FROM extracciones LIKE 'liberacion'")
            existe_liberacion = cursor.fetchone()

            cursor.execute("SHOW COLUMNS FROM extracciones LIKE 'tipo_liberacion'")
            existe_tipo_liberacion = cursor.fetchone()

            # Si no existe 'liberacion', lo agregamos
            if not existe_liberacion:
                cursor.execute("ALTER TABLE extracciones ADD COLUMN liberacion VARCHAR(3)")
                print("Hemos añadido el campo 'liberacion' a la tabla 'extracciones'.")

            # Si no existe 'tipo_liberacion', lo agregamos
            if not existe_tipo_liberacion:
                cursor.execute("ALTER TABLE extracciones ADD COLUMN tipo_liberacion VARCHAR(50)")
                print("Hemos añadido el campo 'tipo_liberacion' a la tabla 'extracciones'.")
        conexion.commit()
    except pymysql.MySQLError as e:
        print(f"Hubo un error al verificar o agregar campos de liberación: {e}")
        conexion.rollback()
    finally:
        conexion.close()

def actualizar_liberacion_noticia(id_noticia, hubo_liberacion, tipo_liberacion):
    """Actualizamos la base de datos con los resultados de la clasificación de liberación."""
    conexion = obtener_conexion_pymysql()
    if not conexion:
        return
    try:
        with conexion.cursor() as cursor:
            sql = """
            UPDATE extracciones 
            SET liberacion = %s, tipo_liberacion = %s 
            WHERE id = %s
            """
            cursor.execute(sql, ('Sí' if hubo_liberacion else 'No', tipo_liberacion, id_noticia))
        conexion.commit()
        print(f"Liberación actualizada para la noticia ID {id_noticia}: {'Sí' if hubo_liberacion else 'No'}, Tipo: {tipo_liberacion}")
    except pymysql.MySQLError as e:
        print(f"Hubo un error al actualizar la liberación: {e}")
        conexion.rollback()
    finally:
        conexion.close()

def clasificar_liberacion_noticia(id_noticia, texto_noticia):
    """Clasificamos si hubo liberación en la noticia y su tipo."""
    hubo_liberacion, tipo_liberacion = clasificar_liberacion(texto_noticia)

    # Mostramos resultados de la clasificación
    print(f"¿Hubo liberación?: {'Sí' if hubo_liberacion else 'No'}")
    print(f"Tipo de liberación: {tipo_liberacion}")

    # Actualizamos la base de datos con los resultados
    actualizar_liberacion_noticia(id_noticia, hubo_liberacion, tipo_liberacion)

def obtener_noticias_liberacion():
    """Recuperamos las noticias que están relacionadas con secuestros para analizar liberación."""
    conexion = obtener_conexion_pymysql()
    if not conexion:
        return []
    try:
        with conexion.cursor() as cursor:
            sql = "SELECT id, noticia_corregida FROM extracciones WHERE relacion_spacy4 = 'sí'"
            cursor.execute(sql)
            resultados = cursor.fetchall()
            return resultados
    except pymysql.MySQLError as e:
        print(f"Hubo un error al obtener noticias para liberación: {e}")
        return []
    finally:
        conexion.close()

def procesar_noticias_liberacion():
    """Procesamos las noticias relacionadas con secuestros para clasificar liberaciones."""
    noticias = obtener_noticias_liberacion()
    if not noticias:
        print("No hay noticias para procesar en la clasificación de liberación.")
        return

    conexion = obtener_conexion_pymysql()
    if not conexion:
        return

    try:
        with conexion.cursor() as cursor:
            for noticia in noticias:
                id_noticia = noticia['id']
                texto_noticia = noticia['noticia_corregida']
                print(f"\n--- Analizando noticia con ID: {id_noticia} ---")
                clasificar_liberacion_noticia(id_noticia, texto_noticia)
    except pymysql.MySQLError as e:
        print(f"Hubo un error al procesar noticias de liberación: {e}")
    finally:
        conexion.close()

# --- SEXTA PARTE: EXTRACIÓN DEL PERFIL DE LA VÍCTIMA ---

def extraer_perfil_victima(texto):
    """Extraemos el perfil de la víctima de una noticia."""
    doc = nlp(texto)
    perfiles_detectados = []
    victimas_unicas = set()

    # Patrones contextuales y verbos de secuestro
    verbos_secuestro = ["secuestro", "secuestrar", "raptar", "privar", "plagiar", "desaparecer", "sustraer"]

    for sent in doc.sents:
        for token in sent:
            if normalizar_texto(token.lemma_) in verbos_secuestro and token.pos_ == "VERB":
                # Detectamos objetos directos, sujetos y complementos que son víctimas
                victimas = []
                if token.dep_ in ("ROOT", "conj"):
                    for child in token.children:
                        if child.dep_ in ("obj", "dobj", "obl", "nsubj:pass", "iobj", "nsubjpass"):
                            victimas.extend(obtener_victimas_desde_token(child))
                for victima in victimas:
                    identidad_victima = f"{victima.text}_{victima.i}"
                    if identidad_victima not in victimas_unicas:
                        victimas_unicas.add(identidad_victima)
                        perfil = analizar_victima(victima, sent)
                        if perfil:
                            perfiles_detectados.append(perfil)

    # Marcamos si hay múltiples víctimas
    multiples_victimas = 'Sí' if len(perfiles_detectados) > 1 else 'No'

    # Consolidamos los perfiles en un único resultado
    perfil_consolidado = consolidar_perfiles(perfiles_detectados)
    perfil_consolidado['multiples_victimas'] = multiples_victimas

    return perfil_consolidado

def obtener_victimas_desde_token(token):
    """Recursivamente obtenemos tokens que representan víctimas desde un token dado."""
    victimas = []
    if token.pos_ in ("NOUN", "PROPN"):
        victimas.append(token)
    for child in token.children:
        victimas.extend(obtener_victimas_desde_token(child))
    return victimas

def analizar_victima(victima_token, sent):
    """Analizamos un token de víctima para extraer su perfil."""
    perfil = {}

    # --- Menor de edad ---
    es_menor, justificacion_menor = determinar_menor_de_edad(victima_token, sent)
    if es_menor is not None:
        perfil['menor_de_edad'] = 'Sí' if es_menor else 'No'
        if justificacion_menor:
            perfil['justificacion_menor_de_edad'] = justificacion_menor

    # --- Edad ---
    edad, justificacion_edad = extraer_edad(victima_token, sent)
    if edad:
        perfil['edad_victima'] = edad
        if int(edad) < 18:
            perfil['menor_de_edad'] = 'Sí'
        if justificacion_edad:
            perfil['justificacion_edad'] = justificacion_edad

    # --- Género ---
    genero, justificacion_genero = determinar_genero(victima_token, sent)
    if genero:
        perfil['genero_victima'] = genero
        if justificacion_genero:
            perfil['justificacion_genero_victima'] = justificacion_genero

    # --- Ocupación ---
    ocupacion, justificacion_ocupacion = extraer_ocupacion(victima_token, sent)
    if ocupacion:
        perfil['ocupacion_victima'] = ocupacion
        if justificacion_ocupacion:
            perfil['justificacion_ocupacion_victima'] = justificacion_ocupacion

    # --- Nacionalidad ---
    nacionalidad, justificacion_nacionalidad = extraer_nacionalidad(victima_token, sent)
    if nacionalidad:
        perfil['nacionalidad_victima'] = nacionalidad
        if justificacion_nacionalidad:
            perfil['justificacion_nacionalidad_victima'] = justificacion_nacionalidad

    return perfil if perfil else None

def consolidar_perfiles(perfiles):
    """Consolidamos múltiples perfiles en uno solo, priorizando la información más completa."""
    perfil_final = {}
    for perfil in perfiles:
        for clave, valor in perfil.items():
            if clave not in perfil_final or not perfil_final[clave]:
                perfil_final[clave] = valor
    return perfil_final

def determinar_menor_de_edad(token_victima, sent):
    """Determinamos si la víctima es menor de edad basado en palabras clave en la oración."""
    palabras_menor = ['niño', 'niña', 'menor', 'adolescente', 'infante', 'bebé', 'chico', 'chica', 'nieto', 'hijo', 'hija', 'menores']
    texto = sent.text.lower()
    for palabra in palabras_menor:
        if palabra in texto:
            return True, f"Palabra clave encontrada: '{palabra}' en texto: '{sent.text.strip()}'"
    return None, None

def extraer_edad(token_victima, sent):
    """Extraemos la edad de la víctima utilizando patrones regex."""
    texto = sent.text
    patrones_edad = [
        rf"{re.escape(token_victima.text)} de (\d{{1,3}}) años\b",
        rf"{re.escape(token_victima.text)} de (\d{{1,3}}) años de edad\b",
        r"\b(\d{1,2}) años\b",
        r"(\d{1,3}) años de edad"
    ]
    for patron in patrones_edad:
        coincidencias = re.findall(patron, texto, re.IGNORECASE)
        if coincidencias:
            return coincidencias[0], f"Patrón encontrado: '{patron}' en texto: '{texto.strip()}'"
    return None, None

def determinar_genero(token_victima, sent):
    """Determinamos el género de la víctima basado en palabras clave en la oración."""
    palabras_masculinas = ['hombre', 'varón', 'niño', 'adolescente', 'joven', 'profesor', 'doctor', 'ingeniero', 'activista', 'alcalde', 'maestro']
    palabras_femeninas = ['mujer', 'fémina', 'niña', 'adolescente', 'joven', 'profesora', 'doctora', 'ingeniera', 'activista', 'alcaldesa', 'maestra']
    texto = sent.text.lower()

    for palabra in palabras_masculinas:
        if palabra in texto:
            return 'Masculino', f"Palabra clave encontrada: '{palabra}' en texto: '{sent.text.strip()}'"
    for palabra in palabras_femeninas:
        if palabra in texto:
            return 'Femenino', f"Palabra clave encontrada: '{palabra}' en texto: '{sent.text.strip()}'"
    return None, None

def extraer_ocupacion(token_victima, sent):
    """Extraemos la ocupación de la víctima utilizando patrones predefinidos."""
    ocupaciones = [
        'alcalde', 'diputado', 'senador', 'gobernador', 'presidente', 'médico', 'doctor', 'enfermero', 'abogado', 'ingeniero',
        'estudiante', 'empresario', 'comerciante', 'profesor', 'periodista', 'policía', 'militar', 'taxista', 'chofer', 'trabajador', 'activista'
    ]
    texto = sent.text.lower()
    for ocupacion in ocupaciones:
        if re.search(rf"\b{ocupacion}\b", texto):
            return ocupacion.capitalize(), f"Ocupación encontrada: '{ocupacion}' en texto: '{sent.text.strip()}'"
    return None, None

def extraer_nacionalidad(token_victima, sent):
    """Extraemos la nacionalidad de la víctima utilizando patrones predefinidos."""
    nacionalidades = ['mexicano', 'mexicana', 'estadounidense', 'canadiense', 'español', 'colombiano', 'argentino', 'venezolano', 'peruano', 'chileno']
    texto = sent.text.lower()
    for nacionalidad in nacionalidades:
        if re.search(rf"\b{nacionalidad}\b", texto):
            return nacionalidad.capitalize(), f"Nacionalidad encontrada: '{nacionalidad}' en texto: '{sent.text.strip()}'"
    return None, None

def verificar_y_agregar_campos_perfil():
    """Nos aseguramos de que las columnas relacionadas con el perfil de la víctima existan y las creamos si no."""
    conexion = obtener_conexion_pymysql()
    if not conexion:
        return
    try:
        with conexion.cursor() as cursor:
            # Verificamos y agregamos campos si no existen
            campos = ['edad_victima', 'menor_de_edad', 'genero_victima', 'ocupacion_victima', 'nacionalidad_victima', 'multiples_victimas',
                      'justificacion_menor_de_edad', 'justificacion_edad', 'justificacion_genero_victima', 'justificacion_ocupacion_victima', 'justificacion_nacionalidad_victima']
            for campo in campos:
                cursor.execute(f"SHOW COLUMNS FROM extracciones LIKE '{campo}'")
                if not cursor.fetchone():
                    # Determinamos el tipo de dato adecuado
                    if 'justificacion' in campo:
                        tipo = "TEXT"
                    elif campo == 'multiples_victimas':
                        tipo = "VARCHAR(3)"
                    else:
                        tipo = "VARCHAR(255)"
                    cursor.execute(f"ALTER TABLE extracciones ADD COLUMN {campo} {tipo}")
                    print(f"Hemos añadido el campo '{campo}' a la tabla 'extracciones'.")
        conexion.commit()
    except pymysql.MySQLError as e:
        print(f"Hubo un error al verificar o agregar campos de perfil: {e}")
        conexion.rollback()
    finally:
        conexion.close()

def actualizar_perfil_noticia(id_noticia, perfil):
    """Actualizamos el perfil de la víctima en la base de datos para una noticia específica."""
    conexion = obtener_conexion_pymysql()
    if not conexion:
        return
    try:
        with conexion.cursor() as cursor:
            sql = """
            UPDATE extracciones SET 
                edad_victima = %s, 
                menor_de_edad = %s, 
                genero_victima = %s, 
                ocupacion_victima = %s, 
                nacionalidad_victima = %s, 
                multiples_victimas = %s,
                justificacion_menor_de_edad = %s,
                justificacion_edad = %s,
                justificacion_genero_victima = %s,
                justificacion_ocupacion_victima = %s,
                justificacion_nacionalidad_victima = %s
            WHERE id = %s
            """
            cursor.execute(sql, (
                perfil.get('edad_victima', ''),
                perfil.get('menor_de_edad', ''),
                perfil.get('genero_victima', ''),
                perfil.get('ocupacion_victima', ''),
                perfil.get('nacionalidad_victima', ''),
                perfil.get('multiples_victimas', 'No'),
                perfil.get('justificacion_menor_de_edad', ''),
                perfil.get('justificacion_edad', ''),
                perfil.get('justificacion_genero_victima', ''),
                perfil.get('justificacion_ocupacion_victima', ''),
                perfil.get('justificacion_nacionalidad_victima', ''),
                id_noticia
            ))
        conexion.commit()
        print(f"Perfil de víctima actualizado para la noticia ID {id_noticia}.")
    except pymysql.MySQLError as e:
        print(f"Hubo un error al actualizar el perfil de la víctima: {e}")
        conexion.rollback()
    finally:
        conexion.close()

def obtener_noticias_perfil():
    """Recuperamos las noticias que están relacionadas con secuestros para analizar el perfil de la víctima."""
    conexion = obtener_conexion_pymysql()
    if not conexion:
        return []
    try:
        with conexion.cursor() as cursor:
            sql = "SELECT id, noticia_corregida FROM extracciones WHERE relacion_spacy4 = 'sí'"
            cursor.execute(sql)
            resultados = cursor.fetchall()
            return resultados
    except pymysql.MySQLError as e:
        print(f"Hubo un error al obtener noticias para perfil de víctima: {e}")
        return []
    finally:
        conexion.close()

def procesar_noticias_perfil():
    """Procesamos las noticias relacionadas con secuestros para extraer el perfil de la víctima."""
    noticias = obtener_noticias_perfil()
    if not noticias:
        print("No hay noticias para procesar en la extracción del perfil de la víctima.")
        return

    conexion = obtener_conexion_pymysql()
    if not conexion:
        return

    try:
        with conexion.cursor() as cursor:
            for noticia in noticias:
                id_noticia = noticia['id']
                texto_noticia = noticia['noticia_corregida']
                print(f"\n--- Analizando noticia con ID: {id_noticia} ---")
                perfil_victima = extraer_perfil_victima(texto_noticia)

                # Mostramos resultados del análisis
                if perfil_victima:
                    print("Perfil de la víctima:")
                    for clave, valor in perfil_victima.items():
                        print(f"- {clave.replace('_', ' ').capitalize()}: {valor}")
                else:
                    print("No se encontró información sobre el perfil de la víctima.")

                # Actualizamos la base de datos con el perfil de la víctima
                actualizar_perfil_noticia(id_noticia, perfil_victima)
    except pymysql.MySQLError as e:
        print(f"Hubo un error al procesar noticias de perfil de víctima: {e}")
    finally:
        conexion.close()

# --- SÉPTIMA PARTE: EXTRACCIÓN DEL TIPO DE SECUESTRO ---

def extraer_tipo_secuestro(texto):
    """Extraemos el tipo de secuestro con análisis contextual."""
    doc = nlp(texto)
    categorias_identificadas = set()
    justificaciones = []

    # Listas de palabras clave y patrones para diferentes tipos de secuestro
    patrones = {
        'Secuestro exprés': {
            'frases_directas': ['secuestro exprés', 'secuestro express', 'secuestro rápido', 'plagio exprés'],
            'indicadores': ['corto tiempo', 'rápido', 'obtener beneficio', 'secuestro breve']
        },
        'Secuestro virtual': {
            'frases_directas': ['secuestro virtual', 'extorsión telefónica', 'secuestro simulado', 'secuestro falso'],
            'indicadores': ['contactar a un pariente', 'pagar un rescate', 'secuestro no se cristaliza', 'extorsión']
        },
        'Secuestro extorsivo': {
            'indicadores': [
                'exigieron rescate', 'exigieron dinero', 'exigió pago', 'demandaron rescate',
                'pidieron rescate', 'exigencia económica', 'exigió dinero', 'pedir una suma monetaria',
                'pedir un beneficio', 'empresario', 'comerciante', 'banquero', 'industrial', 'empresaria',
                'secuestrado', 'plagiado', 'privado de su libertad'
            ],
        },
        'Secuestro político': {
            'roles': [
                'alcalde', 'senador', 'diputado', 'gobernador', 'presidente', 'suplente',
                'ministro', 'regidor', 'funcionario', 'político', 'activista', 'candidato'
            ],
            'indicadores': [
                'crear un entorno inseguro', 'conseguir publicidad', 'gran influencia en las decisiones',
                'decisiones del estado', 'decisiones de otras entidades'
            ],
        },
        'Secuestro por delincuencia organizada': {
            'grupos_criminales': [
                'cártel', 'cartel', 'banda', 'grupo delictivo', 'grupo criminal',
                'delincuencia organizada', 'grupos armados', 'criminales'
            ],
            'acciones': ['secuestró', 'secuestraron', 'plagiaron', 'privaron de la libertad'],
        },
        'Secuestro de migrantes': {
            'victimas': [
                'migrantes', 'inmigrantes', 'centroamericanos', 'migrantes mexicanos', 'migrantes extranjeros'
            ],
            'acciones': ['secuestrados', 'plagiados', 'privados de su libertad'],
        },
        'Secuestro familiar': {
            'relaciones_familiares': [
                'padre', 'madre', 'hijo', 'hija', 'hermano', 'hermana', 'esposo', 'esposa',
                'tío', 'tía', 'abuelo', 'abuela', 'sobrino', 'sobrina', 'primo', 'prima',
                'familiar', 'pariente'
            ],
            'verbos_relacionados': ['secuestrar', 'privar', 'raptar', 'plagiar'],
        },
        'Secuestro de menores': {
            'victimas': [
                'niño', 'niña', 'menor', 'adolescente', 'bebé', 'infante'
            ],
            'verbos_relacionados': ['secuestro', 'secuestrar', 'plagiar', 'raptar', 'privar'],
        },
        'Secuestro simulado': {
            'indicadores': [
                'víctima planeó el secuestro', 'auto-secuestro', 'secuestro falso',
                'simular secuestro', 'secuestro simulado', 'secuestro fingido',
                'la víctima planea el secuestro'
            ],
        },
        'Secuestro con fines de explotación sexual': {
            'indicadores': [
                'dañar su integridad sexual', 'explotación sexual', 'abuso sexual',
                'trata de personas', 'violación', 'prostitución forzada', 'esclavitud sexual'
            ],
            'verbos_relacionados': ['secuestro', 'secuestrar', 'privar', 'plagiar', 'raptar'],
        },
    }

    texto_normalizado = normalizar_texto(texto)

    # Detección de tipos de secuestro mencionados directamente
    for tipo, detalles in patrones.items():
        if 'frases_directas' in detalles:
            for frase in detalles['frases_directas']:
                if frase in texto_normalizado:
                    categorias_identificadas.add(tipo)
                    justificaciones.append(f"Detectamos la frase directa '{frase}' para '{tipo}'.")
                    return tipo, '; '.join(justificaciones)  # Solo asignamos el primer tipo encontrado

    # Análisis de oraciones
    for sent in doc.sents:
        sent_text = sent.text.lower()
        sent_doc = nlp(sent.text)

        # Si ya detectamos un tipo de secuestro, dejamos de analizar
        if categorias_identificadas:
            break

        # Secuestro con fines de explotación sexual
        if 'Secuestro con fines de explotación sexual' not in categorias_identificadas:
            if any(indicador in sent_text for indicador in patrones['Secuestro con fines de explotación sexual']['indicadores']):
                categorias_identificadas.add('Secuestro con fines de explotación sexual')
                justificaciones.append(f"Detectamos un indicador de explotación sexual en: '{sent.text.strip()}'.")
                return 'Secuestro con fines de explotación sexual', '; '.join(justificaciones)

        # Secuestro simulado
        if 'Secuestro simulado' not in categorias_identificadas:
            if any(indicador in sent_text for indicador in patrones['Secuestro simulado']['indicadores']):
                categorias_identificadas.add('Secuestro simulado')
                justificaciones.append(f"Detectamos un indicador de secuestro simulado en: '{sent.text.strip()}'.")
                return 'Secuestro simulado', '; '.join(justificaciones)

        # Secuestro de menores
        if 'Secuestro de menores' not in categorias_identificadas:
            for token in sent_doc:
                if token.lemma_ in patrones['Secuestro de menores']['verbos_relacionados'] and token.pos_ == 'VERB':
                    objeto = None
                    # Obtenemos el objeto directo del verbo
                    for child in token.children:
                        if child.dep_ in ('dobj', 'obj'):
                            objeto = child
                            break
                    if objeto:
                        objeto_text = objeto.text.lower()
                        # Verificamos si el objeto es un menor
                        if any(victima in objeto_text for victima in patrones['Secuestro de menores']['victimas']):
                            categorias_identificadas.add('Secuestro de menores')
                            justificaciones.append(f"Detectamos un menor como víctima en: '{sent.text.strip()}'.")
                            return 'Secuestro de menores', '; '.join(justificaciones)

        # Secuestro político
        if 'Secuestro político' not in categorias_identificadas:
            for ent in sent_doc.ents:
                if ent.label_ == 'PER' and any(role in ent.text.lower() for role in patrones['Secuestro político']['roles']):
                    if any(verb.lemma_ in ['secuestro', 'privar', 'plagiar', 'raptar'] for verb in sent_doc if verb.pos_ == 'VERB'):
                        categorias_identificadas.add('Secuestro político')
                        justificaciones.append(f"Detectamos un verbo de secuestro relacionado con una persona política '{ent.text}'.")
                        return 'Secuestro político', '; '.join(justificaciones)
            if any(indicador in sent_text for indicador in patrones['Secuestro político'].get('indicadores', [])):
                categorias_identificadas.add('Secuestro político')
                justificaciones.append(f"Detectamos un indicador político en: '{sent.text.strip()}'.")
                return 'Secuestro político', '; '.join(justificaciones)

        # Secuestro por delincuencia organizada
        if 'Secuestro por delincuencia organizada' not in categorias_identificadas:
            if any(grupo in sent_text for grupo in patrones['Secuestro por delincuencia organizada']['grupos_criminales']) and any(action in sent_text for action in patrones['Secuestro por delincuencia organizada']['acciones']):
                categorias_identificadas.add('Secuestro por delincuencia organizada')
                justificaciones.append(f"Detectamos un grupo criminal y una acción de secuestro en: '{sent.text.strip()}'.")
                return 'Secuestro por delincuencia organizada', '; '.join(justificaciones)

        # Secuestro extorsivo (combinado)
        if 'Secuestro extorsivo' not in categorias_identificadas:
            if any(indicador in sent_text for indicador in patrones['Secuestro extorsivo']['indicadores']):
                categorias_identificadas.add('Secuestro extorsivo')
                justificaciones.append(f"Detectamos un indicador de secuestro extorsivo en: '{sent.text.strip()}'.")
                return 'Secuestro extorsivo', '; '.join(justificaciones)

        # Secuestro familiar
        if 'Secuestro familiar' not in categorias_identificadas:
            for token in sent_doc:
                if token.lemma_ in patrones['Secuestro familiar']['verbos_relacionados'] and token.pos_ == 'VERB':
                    sujeto = None
                    objeto = None
                    # Obtenemos el sujeto y el objeto del verbo
                    for child in token.children:
                        if child.dep_ in ('nsubj', 'nsubj:pass'):
                            sujeto = child
                        elif child.dep_ in ('dobj', 'obj'):
                            objeto = child
                    if sujeto and any(rel in sujeto.text.lower() for rel in patrones['Secuestro familiar']['relaciones_familiares']):
                        categorias_identificadas.add('Secuestro familiar')
                        justificaciones.append(f"Detectamos a un familiar '{sujeto.text}' como perpetrador en: '{sent.text.strip()}'.")
                        return 'Secuestro familiar', '; '.join(justificaciones)
                    # En caso de voz pasiva, buscamos el agente
                    for child in token.children:
                        if child.dep_ == 'agent':
                            agente = child.text.lower()
                            if any(rel in agente for rel in patrones['Secuestro familiar']['relaciones_familiares']):
                                categorias_identificadas.add('Secuestro familiar')
                                justificaciones.append(f"Detectamos a un familiar '{child.text}' como perpetrador en voz pasiva en: '{sent.text.strip()}'.")
                                return 'Secuestro familiar', '; '.join(justificaciones)

    # Si no encontramos ningún tipo específico, pero se menciona alguna conjugación de los verbos relacionados, asignamos "Secuestro general"
    if not categorias_identificadas:
        verbos_secuestro = {'secuestro', 'secuestrar', 'privar', 'plagiar', 'raptar', 'plagio', 'rapto', 'privado', 'privada'}
        lemmas_en_texto = [token.lemma_.lower() for token in doc]
        if any(lemma in verbos_secuestro for lemma in lemmas_en_texto):
            categorias_identificadas.add('Secuestro general')
            justificaciones.append("No detectamos un tipo específico, pero se mencionó 'secuestro' o 'privar' en alguna de sus formas.")
        else:
            justificaciones.append("No detectamos 'secuestro' o 'privar' en el texto.")

    # Retornamos el primer tipo de secuestro identificado o 'Secuestro general' si no se encontró ninguno específico
    tipo_secuestro = next(iter(categorias_identificadas)) if categorias_identificadas else ''
    return tipo_secuestro, '; '.join(justificaciones)

def verificar_y_agregar_campos_tipo_secuestro():
    """Nos aseguramos de que las columnas 'tipo_secuestro' y 'justificacion_tipo_secuestro' existan y las creamos si no."""
    conexion = obtener_conexion_pymysql()
    if not conexion:
        return
    try:
        with conexion.cursor() as cursor:
            # Verificamos y agregamos el campo 'tipo_secuestro'
            cursor.execute("SHOW COLUMNS FROM extracciones LIKE 'tipo_secuestro'")
            existe_tipo_secuestro = cursor.fetchone()

            # Verificamos y agregamos el campo 'justificacion_tipo_secuestro'
            cursor.execute("SHOW COLUMNS FROM extracciones LIKE 'justificacion_tipo_secuestro'")
            existe_justificacion = cursor.fetchone()

            # Si no existe 'tipo_secuestro', lo agregamos
            if not existe_tipo_secuestro:
                cursor.execute("ALTER TABLE extracciones ADD COLUMN tipo_secuestro VARCHAR(255)")
                print("Hemos añadido el campo 'tipo_secuestro' a la tabla 'extracciones'.")

            # Si no existe 'justificacion_tipo_secuestro', lo agregamos
            if not existe_justificacion:
                cursor.execute("ALTER TABLE extracciones ADD COLUMN justificacion_tipo_secuestro TEXT")
                print("Hemos añadido el campo 'justificacion_tipo_secuestro' a la tabla 'extracciones'.")
        conexion.commit()
    except pymysql.MySQLError as e:
        print(f"Hubo un error al verificar o agregar campos de tipo de secuestro: {e}")
        conexion.rollback()
    finally:
        conexion.close()

def actualizar_tipo_secuestro(id_noticia, tipo_secuestro, justificacion):
    """Actualizamos la base de datos con el tipo de secuestro y su justificación."""
    conexion = obtener_conexion_pymysql()
    if not conexion:
        return
    try:
        with conexion.cursor() as cursor:
            sql = """
            UPDATE extracciones 
            SET tipo_secuestro = %s, justificacion_tipo_secuestro = %s 
            WHERE id = %s
            """
            cursor.execute(sql, (tipo_secuestro, justificacion, id_noticia))
        conexion.commit()
        print(f"Tipo de secuestro actualizado para la noticia ID {id_noticia}: {tipo_secuestro}")
    except pymysql.MySQLError as e:
        print(f"Hubo un error al actualizar el tipo de secuestro: {e}")
        conexion.rollback()
    finally:
        conexion.close()

def extraer_tipo_secuestro(texto):
    """Extraemos el tipo de secuestro con análisis contextual."""
    doc = nlp(texto)
    categorias_identificadas = set()
    justificaciones = []

    # Listas de palabras clave y patrones para diferentes tipos de secuestro
    patrones = {
        'Secuestro exprés': {
            'frases_directas': ['secuestro exprés', 'secuestro express', 'secuestro rápido', 'plagio exprés'],
            'indicadores': ['corto tiempo', 'rápido', 'obtener beneficio', 'secuestro breve']
        },
        'Secuestro virtual': {
            'frases_directas': ['secuestro virtual', 'extorsión telefónica', 'secuestro simulado', 'secuestro falso'],
            'indicadores': ['contactar a un pariente', 'pagar un rescate', 'secuestro no se cristaliza', 'extorsión']
        },
        'Secuestro extorsivo': {
            'indicadores': [
                'exigieron rescate', 'exigieron dinero', 'exigió pago', 'demandaron rescate',
                'pidieron rescate', 'exigencia económica', 'exigió dinero', 'pedir una suma monetaria',
                'pedir un beneficio', 'empresario', 'comerciante', 'banquero', 'industrial', 'empresaria',
                'secuestrado', 'plagiado', 'privado de su libertad'
            ],
        },
        'Secuestro político': {
            'roles': [
                'alcalde', 'senador', 'diputado', 'gobernador', 'presidente', 'suplente',
                'ministro', 'regidor', 'funcionario', 'político', 'activista', 'candidato'
            ],
            'indicadores': [
                'crear un entorno inseguro', 'conseguir publicidad', 'gran influencia en las decisiones',
                'decisiones del estado', 'decisiones de otras entidades'
            ],
        },
        'Secuestro por delincuencia organizada': {
            'grupos_criminales': [
                'cártel', 'cartel', 'banda', 'grupo delictivo', 'grupo criminal',
                'delincuencia organizada', 'grupos armados', 'criminales'
            ],
            'acciones': ['secuestró', 'secuestraron', 'plagiaron', 'privaron de la libertad'],
        },
        'Secuestro de migrantes': {
            'victimas': [
                'migrantes', 'inmigrantes', 'centroamericanos', 'migrantes mexicanos', 'migrantes extranjeros'
            ],
            'acciones': ['secuestrados', 'plagiados', 'privados de su libertad'],
        },
        'Secuestro familiar': {
            'relaciones_familiares': [
                'padre', 'madre', 'hijo', 'hija', 'hermano', 'hermana', 'esposo', 'esposa',
                'tío', 'tía', 'abuelo', 'abuela', 'sobrino', 'sobrina', 'primo', 'prima',
                'familiar', 'pariente'
            ],
            'verbos_relacionados': ['secuestrar', 'privar', 'raptar', 'plagiar'],
        },
        'Secuestro de menores': {
            'victimas': [
                'niño', 'niña', 'menor', 'adolescente', 'bebé', 'infante'
            ],
            'verbos_relacionados': ['secuestro', 'secuestrar', 'plagiar', 'raptar', 'privar'],
        },
        'Secuestro simulado': {
            'indicadores': [
                'víctima planeó el secuestro', 'auto-secuestro', 'secuestro falso',
                'simular secuestro', 'secuestro simulado', 'secuestro fingido',
                'la víctima planea el secuestro'
            ],
        },
        'Secuestro con fines de explotación sexual': {
            'indicadores': [
                'dañar su integridad sexual', 'explotación sexual', 'abuso sexual',
                'trata de personas', 'violación', 'prostitución forzada', 'esclavitud sexual'
            ],
            'verbos_relacionados': ['secuestro', 'secuestrar', 'privar', 'plagiar', 'raptar'],
        },
    }

    texto_normalizado = normalizar_texto(texto)

    # Detección de tipos de secuestro mencionados directamente
    for tipo, detalles in patrones.items():
        if 'frases_directas' in detalles:
            for frase in detalles['frases_directas']:
                if frase in texto_normalizado:
                    categorias_identificadas.add(tipo)
                    justificaciones.append(f"Detectamos la frase directa '{frase}' para '{tipo}'.")
                    return tipo, '; '.join(justificaciones)  # Solo asignamos el primer tipo encontrado

    # Análisis de oraciones
    for sent in doc.sents:
        sent_text = sent.text.lower()
        sent_doc = nlp(sent.text)

        # Si ya detectamos un tipo de secuestro, dejamos de analizar
        if categorias_identificadas:
            break

        # Secuestro con fines de explotación sexual
        if 'Secuestro con fines de explotación sexual' not in categorias_identificadas:
            if any(indicador in sent_text for indicador in patrones['Secuestro con fines de explotación sexual']['indicadores']):
                categorias_identificadas.add('Secuestro con fines de explotación sexual')
                justificaciones.append(f"Detectamos un indicador de explotación sexual en: '{sent.text.strip()}'.")
                return 'Secuestro con fines de explotación sexual', '; '.join(justificaciones)

        # Secuestro simulado
        if 'Secuestro simulado' not in categorias_identificadas:
            if any(indicador in sent_text for indicador in patrones['Secuestro simulado']['indicadores']):
                categorias_identificadas.add('Secuestro simulado')
                justificaciones.append(f"Detectamos un indicador de secuestro simulado en: '{sent.text.strip()}'.")
                return 'Secuestro simulado', '; '.join(justificaciones)

        # Secuestro de menores
        if 'Secuestro de menores' not in categorias_identificadas:
            for token in sent_doc:
                if token.lemma_ in patrones['Secuestro de menores']['verbos_relacionados'] and token.pos_ == 'VERB':
                    objeto = None
                    # Obtenemos el objeto directo del verbo
                    for child in token.children:
                        if child.dep_ in ('dobj', 'obj'):
                            objeto = child
                            break
                    if objeto:
                        objeto_text = objeto.text.lower()
                        # Verificamos si el objeto es un menor
                        if any(victima in objeto_text for victima in patrones['Secuestro de menores']['victimas']):
                            categorias_identificadas.add('Secuestro de menores')
                            justificaciones.append(f"Detectamos un menor como víctima en: '{sent.text.strip()}'.")
                            return 'Secuestro de menores', '; '.join(justificaciones)

        # Secuestro político
        if 'Secuestro político' not in categorias_identificadas:
            for ent in sent_doc.ents:
                if ent.label_ == 'PER' and any(role in ent.text.lower() for role in patrones['Secuestro político']['roles']):
                    if any(verb.lemma_ in ['secuestro', 'privar', 'plagiar', 'raptar'] for verb in sent_doc if verb.pos_ == 'VERB'):
                        categorias_identificadas.add('Secuestro político')
                        justificaciones.append(f"Detectamos un verbo de secuestro relacionado con una persona política '{ent.text}'.")
                        return 'Secuestro político', '; '.join(justificaciones)
            if any(indicador in sent_text for indicador in patrones['Secuestro político'].get('indicadores', [])):
                categorias_identificadas.add('Secuestro político')
                justificaciones.append(f"Detectamos un indicador político en: '{sent.text.strip()}'.")
                return 'Secuestro político', '; '.join(justificaciones)

        # Secuestro por delincuencia organizada
        if 'Secuestro por delincuencia organizada' not in categorias_identificadas:
            if any(grupo in sent_text for grupo in patrones['Secuestro por delincuencia organizada']['grupos_criminales']) and any(action in sent_text for action in patrones['Secuestro por delincuencia organizada']['acciones']):
                categorias_identificadas.add('Secuestro por delincuencia organizada')
                justificaciones.append(f"Detectamos un grupo criminal y una acción de secuestro en: '{sent.text.strip()}'.")
                return 'Secuestro por delincuencia organizada', '; '.join(justificaciones)

        # Secuestro extorsivo (combinado)
        if 'Secuestro extorsivo' not in categorias_identificadas:
            if any(indicador in sent_text for indicador in patrones['Secuestro extorsivo']['indicadores']):
                categorias_identificadas.add('Secuestro extorsivo')
                justificaciones.append(f"Detectamos un indicador de secuestro extorsivo en: '{sent.text.strip()}'.")
                return 'Secuestro extorsivo', '; '.join(justificaciones)

        # Secuestro familiar
        if 'Secuestro familiar' not in categorias_identificadas:
            for token in sent_doc:
                if token.lemma_ in patrones['Secuestro familiar']['verbos_relacionados'] and token.pos_ == 'VERB':
                    sujeto = None
                    objeto = None
                    # Obtenemos el sujeto y el objeto del verbo
                    for child in token.children:
                        if child.dep_ in ('nsubj', 'nsubj:pass'):
                            sujeto = child
                        elif child.dep_ in ('dobj', 'obj'):
                            objeto = child
                    if sujeto and any(rel in sujeto.text.lower() for rel in patrones['Secuestro familiar']['relaciones_familiares']):
                        categorias_identificadas.add('Secuestro familiar')
                        justificaciones.append(f"Detectamos a un familiar '{sujeto.text}' como perpetrador en: '{sent.text.strip()}'.")
                        return 'Secuestro familiar', '; '.join(justificaciones)
                    # En caso de voz pasiva, buscamos el agente
                    for child in token.children:
                        if child.dep_ == 'agent':
                            agente = child.text.lower()
                            if any(rel in agente for rel in patrones['Secuestro familiar']['relaciones_familiares']):
                                categorias_identificadas.add('Secuestro familiar')
                                justificaciones.append(f"Detectamos a un familiar '{child.text}' como perpetrador en voz pasiva en: '{sent.text.strip()}'.")
                                return 'Secuestro familiar', '; '.join(justificaciones)

    # Si no encontramos ningún tipo específico, pero se menciona alguna conjugación de los verbos relacionados, asignamos "Secuestro general"
    if not categorias_identificadas:
        verbos_secuestro = {'secuestro', 'secuestrar', 'privar', 'plagiar', 'raptar', 'plagio', 'rapto', 'privado', 'privada'}
        lemmas_en_texto = [token.lemma_.lower() for token in doc]
        if any(lemma in verbos_secuestro for lemma in lemmas_en_texto):
            categorias_identificadas.add('Secuestro general')
            justificaciones.append("No detectamos un tipo específico, pero se mencionó 'secuestro' o 'privar' en alguna de sus formas.")
        else:
            justificaciones.append("No detectamos 'secuestro' o 'privar' en el texto.")

    # Retornamos el primer tipo de secuestro identificado o 'Secuestro general' si no se encontró ninguno específico
    tipo_secuestro = next(iter(categorias_identificadas)) if categorias_identificadas else ''
    return tipo_secuestro, '; '.join(justificaciones)

def verificar_y_agregar_campos_tipo_secuestro():
    """Nos aseguramos de que las columnas 'tipo_secuestro' y 'justificacion_tipo_secuestro' existan y las creamos si no."""
    conexion = obtener_conexion_pymysql()
    if not conexion:
        return
    try:
        with conexion.cursor() as cursor:
            # Verificamos y agregamos el campo 'tipo_secuestro'
            cursor.execute("SHOW COLUMNS FROM extracciones LIKE 'tipo_secuestro'")
            existe_tipo_secuestro = cursor.fetchone()

            # Verificamos y agregamos el campo 'justificacion_tipo_secuestro'
            cursor.execute("SHOW COLUMNS FROM extracciones LIKE 'justificacion_tipo_secuestro'")
            existe_justificacion = cursor.fetchone()

            # Si no existe 'tipo_secuestro', lo agregamos
            if not existe_tipo_secuestro:
                cursor.execute("ALTER TABLE extracciones ADD COLUMN tipo_secuestro VARCHAR(255)")
                print("Hemos añadido el campo 'tipo_secuestro' a la tabla 'extracciones'.")

            # Si no existe 'justificacion_tipo_secuestro', lo agregamos
            if not existe_justificacion:
                cursor.execute("ALTER TABLE extracciones ADD COLUMN justificacion_tipo_secuestro TEXT")
                print("Hemos añadido el campo 'justificacion_tipo_secuestro' a la tabla 'extracciones'.")
        conexion.commit()
    except pymysql.MySQLError as e:
        print(f"Hubo un error al verificar o agregar campos de tipo de secuestro: {e}")
        conexion.rollback()
    finally:
        conexion.close()

def actualizar_tipo_secuestro(id_noticia, tipo_secuestro, justificacion):
    """Actualizamos la base de datos con el tipo de secuestro y su justificación."""
    conexion = obtener_conexion_pymysql()
    if not conexion:
        return
    try:
        with conexion.cursor() as cursor:
            sql = """
            UPDATE extracciones 
            SET tipo_secuestro = %s, justificacion_tipo_secuestro = %s 
            WHERE id = %s
            """
            cursor.execute(sql, (tipo_secuestro, justificacion, id_noticia))
        conexion.commit()
        print(f"Tipo de secuestro actualizado para la noticia ID {id_noticia}: {tipo_secuestro}")
    except pymysql.MySQLError as e:
        print(f"Hubo un error al actualizar el tipo de secuestro: {e}")
        conexion.rollback()
    finally:
        conexion.close()

def obtener_noticias_tipo_secuestro():
    """Recuperamos las noticias que están relacionadas con secuestros para analizar el tipo de secuestro."""
    conexion = obtener_conexion_pymysql()
    if not conexion:
        return []
    try:
        with conexion.cursor() as cursor:
            sql = "SELECT id, noticia_corregida FROM extracciones WHERE relacion_spacy4 = 'sí'"
            cursor.execute(sql)
            resultados = cursor.fetchall()
            return resultados
    except pymysql.MySQLError as e:
        print(f"Hubo un error al obtener noticias para tipo de secuestro: {e}")
        return []
    finally:
        conexion.close()

def procesar_noticias_tipo_secuestro():
    """Procesamos las noticias relacionadas con secuestros para extraer el tipo de secuestro."""
    noticias = obtener_noticias_tipo_secuestro()
    if not noticias:
        print("No hay noticias para procesar en la extracción del tipo de secuestro.")
        return

    conexion = obtener_conexion_pymysql()
    if not conexion:
        return

    try:
        with conexion.cursor() as cursor:
            for noticia in noticias:
                id_noticia = noticia['id']
                texto_noticia = noticia['noticia_corregida']
                print(f"\n--- Analizando noticia con ID: {id_noticia} ---")
                tipo_secuestro, justificacion = extraer_tipo_secuestro(texto_noticia)

                # Mostramos resultados del análisis
                print(f"Tipo de secuestro detectado: {tipo_secuestro}")
                print(f"Justificación: {justificacion}")

                # Actualizamos la base de datos con el tipo de secuestro y su justificación
                actualizar_tipo_secuestro(id_noticia, tipo_secuestro, justificacion)
    except pymysql.MySQLError as e:
        print(f"Hubo un error al procesar noticias de tipo de secuestro: {e}")
    finally:
        conexion.close()

# --- OCTAVA PARTE: DETECCIÓN Y MARCADO DE NOTICIAS REPETIDAS ---

def verificar_y_agregar_campos_repetidas():
    """Nos aseguramos de que la columna 'noticias_repetidas' exista y la creamos si no."""
    conexion = obtener_conexion_pymysql()
    if not conexion:
        return
    try:
        with conexion.cursor() as cursor:
            cursor.execute("SHOW COLUMNS FROM extracciones LIKE 'noticias_repetidas';")
            result = cursor.fetchone()
            if not result:
                cursor.execute("""
                    ALTER TABLE extracciones
                    ADD COLUMN noticias_repetidas TINYINT(1) DEFAULT 0;
                """)
                print("Hemos añadido el campo 'noticias_repetidas' a la tabla 'extracciones'.")
        conexion.commit()
    except pymysql.MySQLError as e:
        print(f"Hubo un error al verificar o agregar el campo 'noticias_repetidas': {e}")
        conexion.rollback()
    finally:
        conexion.close()

def obtener_noticias_repetidas():
    """Recuperamos las noticias relacionadas con secuestros para detectar duplicados."""
    conexion = obtener_conexion_pymysql()
    if not conexion:
        return []
    try:
        with conexion.cursor() as cursor:
            # Seleccionamos solo las noticias donde relacion_spacy4 = 'sí' y no están marcadas como repetidas
            sql = """
            SELECT 
                id,
                municipio,
                estado,
                pais,
                mes_secuestro,
                año_secuestro,
                tipo_secuestro,
                captor,
                lugar,
                captura
            FROM extracciones
            WHERE relacion_spacy4 = 'sí' AND (noticias_repetidas IS NULL OR noticias_repetidas <> 1);
            """
            cursor.execute(sql)
            resultados = cursor.fetchall()
            return resultados
    except pymysql.MySQLError as e:
        print(f"Hubo un error al obtener noticias para detección de duplicados: {e}")
        return []
    finally:
        conexion.close()

def procesar_noticias_repetidas():
    """Procesamos las noticias para detectar y marcar duplicados."""
    verificar_y_agregar_campos_repetidas()
    noticias = obtener_noticias_repetidas()
    if not noticias:
        print("No hay noticias para procesar en la detección de duplicados.")
        return

    # Creamos una lista de diccionarios para facilitar el manejo
    columns = ['id', 'municipio', 'estado', 'pais', 'mes_secuestro', 'año_secuestro', 'tipo_secuestro', 'captor', 'lugar', 'captura']
    data = [dict(zip(columns, row.values())) for row in noticias]

    # Creamos un diccionario para agrupar posibles duplicados
    potential_duplicates = {}
    for row in data:
        key = (
            row['municipio'],
            row['estado'],
            row['pais'],
            row['mes_secuestro'],
            row['año_secuestro']
        )
        if key in potential_duplicates:
            potential_duplicates[key].append(row)
        else:
            potential_duplicates[key] = [row]

    # Identificamos y marcamos duplicados
    duplicates_to_mark = []
    for group in potential_duplicates.values():
        if len(group) > 1:
            # Para cada grupo con los mismos campos clave
            seen = {}
            for entry in group:
                # Creamos una subclave con los campos adicionales
                sub_key = (
                    entry['tipo_secuestro'],
                    entry['captor'],
                    entry['lugar'],
                    entry['captura']
                )
                if sub_key in seen:
                    # Si ya hemos visto esta combinación, es un duplicado
                    duplicates_to_mark.append(entry['id'])
                else:
                    # Marcamos esta combinación como vista
                    seen[sub_key] = entry['id']

    if duplicates_to_mark:
        # Marcamos los duplicados en la base de datos
        conexion = obtener_conexion_pymysql()
        if not conexion:
            return
        try:
            with conexion.cursor() as cursor:
                format_strings = ','.join(['%s'] * len(duplicates_to_mark))
                sql = f"""
                    UPDATE extracciones
                    SET noticias_repetidas = 1
                    WHERE id IN ({format_strings});
                """
                cursor.execute(sql, duplicates_to_mark)
            conexion.commit()
            print(f"Noticias repetidas marcadas exitosamente. Total de noticias marcadas: {len(duplicates_to_mark)}")
        except pymysql.MySQLError as e:
            print(f"Hubo un error al marcar noticias repetidas: {e}")
            conexion.rollback()
        finally:
            conexion.close()
    else:
        print("No se encontraron noticias repetidas para marcar.")

# --- NOVENA PARTE: FILTRADO Y EXPORTACIÓN DE DATOS CON PANDAS Y SQLALCHEMY ---

def filtrar_y_exportar_datos():
    """Filtramos los datos según criterios específicos y los exportamos a una nueva tabla."""
    engine = obtener_conexion_sqlalchemy()
    if not engine:
        return

    # Definimos la consulta SQL para extraer los datos con los filtros iniciales
    query = """
    SELECT
        id,
        pais,
        estado,
        municipio,
        liberacion,
        tipo_liberacion,
        mes_secuestro,
        año_secuestro,
        captor,
        lugar,
        captura,
        tipo_secuestro
    FROM extracciones
    WHERE relacion_spacy4 = 'sí'
      AND (noticias_repetidas IS NULL OR noticias_repetidas <> 1)
      AND año_secuestro > '2015'
      AND pais = 'México'  -- Filtro adicional para el país México
    """

    try:
        # Leemos los datos desde la base de datos
        df = pd.read_sql(query, engine)

        # Definimos los campos requeridos para el análisis
        campos_requeridos = [
            'pais', 'estado', 'municipio', 'liberacion', 'tipo_liberacion',
            'mes_secuestro', 'año_secuestro', 'captor', 'lugar', 'captura', 'tipo_secuestro'
        ]

        # Filtramos los registros que no tienen valores nulos o vacíos en los campos requeridos
        df_filtered = df.dropna(subset=campos_requeridos)
        df_filtered = df_filtered[(df_filtered[campos_requeridos] != '').all(axis=1)]

        # Opcional: Resetear el índice si es necesario
        df_filtered.reset_index(drop=True, inplace=True)

        # Creamos una nueva tabla en la base de datos con los datos filtrados
        # Puedes elegir el nombre de la nueva tabla, por ejemplo, 'extracciones_filtradas_mexico'
        df_filtered.to_sql('extracciones_filtradas', con=engine, if_exists='replace', index=False)
        print("Datos filtrados y exportados exitosamente a la tabla 'extracciones_filtradas'.")
    except Exception as e:
        print(f"Hubo un error al filtrar y exportar datos: {e}")
    finally:
        engine.dispose()

# --- EJECUCIÓN DEL PROGRAMA ---

if __name__ == "__main__":
    # 1) Limpiamos noticias
    limpiar_noticias()

    # 2) Determinamos si las noticias están relacionadas con secuestros
    procesar_noticias_secuestro()

    # 3) Aseguramos que existan campos geograficos
    agregar_campos_geograficos()

    # 4) Extraemos ubicación de las noticias relacionadas con secuestros
    procesar_noticias_lugares()

    # 5) Verificamos y creamos campos de captura
    verificar_y_crear_campos_captura()

    # 6) Detectamos y analizamos métodos de captura en la
    procesar_noticias_captura()

    # 7) Verificamos y agregamos campos de liberación
    verificar_y_agregar_campos_liberacion()

    # 8) Clasificamos liberación en
    procesar_noticias_liberacion()

    # 9) Verificamos y agregamos campos de tipo de secuestro
    verificar_y_agregar_campos_tipo_secuestro()

    # 10) Extraemos tipo de secuestro en las

    # 11) Verificamos y agregamos campos de perfil de víctima
    verificar_y_agregar_campos_perfil()

    # 12) Extraemos perfil de la víctima e
    procesar_noticias_perfil()

    # 13) Detección y marcado de noticias repetidas
    procesar_noticias_repetidas()

    # 14) Filtramos y exportamos dato
    filtrar_y_exportar_datos()
