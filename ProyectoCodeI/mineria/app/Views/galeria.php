<?= $this->include('templates/header'); ?>

<div class="container mt-5"> 
    <h1 class="text-center mb-4">Resultados de Análisis</h1>

    <!-- Subencabezado "Tendencias" -->
    <h2 class="mt-5 mb-4">Tendencias</h2>
    <div class="row">
        <!-- Imagen 1 -->
        <div class="col-md-4 mb-4">
            <div class="card h-100">
                <img src="<?= base_url('images/tendencia_secuestros.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 1" data-toggle="modal" data-target="#modalImagen1">
                <div class="card-body">
                    <h5 class="card-title">Tendencias secuestros</h5>
                    <!-- Descripción intercambiada -->
                    <p class="card-text">En esta primera imagen tenemos una muestra de la tendencia de los secuestros durante el 2016 y octubre de 2024.</p>
                </div>
            </div>
        </div>

        <!-- Modal Imagen 1 -->
        <div class="modal fade" id="modalImagen1" tabindex="-1" aria-labelledby="modalLabel1" aria-hidden="true">
            <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
                <div class="modal-content">
                    <div class="modal-header">
                        <h5 class="modal-title" id="modalLabel1">Tendencia Secuestros</h5>
                        <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                            <span aria-hidden="true">&times;</span>
                        </button>
                    </div>
                    <div class="modal-body">
                        <img src="<?= base_url('images/tendencia_secuestros.png'); ?>" class="img-fluid" alt="Imagen 1">
                        <!-- Descripción intercambiada -->
                        <p class="mt-3">La imagen muestra la tendencia mensual de los casos de secuestro en México desde 2016 hasta noviembre de 2024. La línea azul conecta los datos mensuales, y los puntos rojos resaltan los valores registrados en cada mes, evidenciando cambios en la cantidad de casos extraídos en un periodo de tiempo.</p>
                    </div>
                </div>
            </div>
        </div>
        <!-- Fin del Modal Imagen 1 -->

        <!-- Imagen 2 -->
        <div class="col-md-4 mb-4">
            <div class="card h-100">
                <img src="<?= base_url('images/tendencia_promedio.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 2" data-toggle="modal" data-target="#modalImagen2">
                <div class="card-body">
                    <h5 class="card-title">Tendencia de secuestros promedio mensual por año</h5>
                    <!-- Descripción intercambiada -->
                    <p class="card-text">La gráfica representa el promedio mensual de secuestros por año de 2016 a 2024, mostrando fluctuaciones anuales en los valores registrados.</p>
                </div>
            </div>
        </div>

        <!-- Modal Imagen 2 -->
        <div class="modal fade" id="modalImagen2" tabindex="-1" aria-labelledby="modalLabel2" aria-hidden="true">
            <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
                <div class="modal-content">
                    <div class="modal-header">
                        <h5 class="modal-title" id="modalLabel2">Tendencia de secuestros promedio mensual por año</h5>
                        <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                            <span aria-hidden="true">&times;</span>
                        </button>
                    </div>
                    <div class="modal-body">
                        <img src="<?= base_url('images/tendencia_promedio.png'); ?>" class="img-fluid" alt="Imagen 2">
                        <!-- Descripción intercambiada -->
                        <p class="mt-3">En esta gráfica se muestra el promedio mensual de secuestros por año desde 2016 hasta noviembre de 2024. Cada punto rojo representa el promedio mensual de secuestros para un año específico, mientras que la línea segmentada conecta los datos anuales.</p>
                    </div>
                </div>
            </div>
        </div>
        <!-- Fin del Modal Imagen 2 -->

        <!-- Imagen 3 -->
        <div class="col-md-4 mb-4">
            <div class="card h-100">
                <img src="<?= base_url('images/tendencia_inegi.png'); ?>" class="card-img-top img-thumbnail" alt="Tendencia de secuestros ENVIPE 2024" data-toggle="modal" data-target="#modalImagen3">
                <div class="card-body">
                    <h5 class="card-title">Tendencia de secuestros ENVIPE 2024</h5>
                    <!-- Descripción intercambiada -->
                    <p class="card-text">Gráfica del INEGI que detalla víctimas y delitos de secuestro en México entre 2012 y 2023, destacando picos en 2013 y fluctuaciones en el tiempo. Fuente: <a href="https://www.inegi.org.mx/contenidos/saladeprensa/boletines/2024/ENVIPE/ENVIPE_24.pdf" target="_blank">INEGI</a>.</p>
                </div>
            </div>
        </div>

        <!-- Modal Imagen 3 -->
        <div class="modal fade" id="modalImagen3" tabindex="-1" aria-labelledby="modalLabel3" aria-hidden="true">
            <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
                <div class="modal-content">
                    <div class="modal-header">
                        <h5 class="modal-title" id="modalLabel3">Tendencia de secuestros ENVIPE 2024</h5>
                        <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                            <span aria-hidden="true">&times;</span>
                        </button>
                    </div>
                    <div class="modal-body">
                        <img src="<?= base_url('images/tendencia_inegi.png'); ?>" class="img-fluid" alt="Tendencia de secuestros ENVIPE 2024">
                        <!-- Descripción intercambiada -->
                        <p class="mt-3">La gráfica del INEGI, extraída de la ENVIPE 2024, presenta las estimaciones anuales de víctimas y delitos de secuestro en México de 2012 a 2023. La línea azul claro muestra las víctimas, mientras que la azul oscuro corresponde a los delitos, ambas con intervalos de confianza. Fuente: <a href="https://www.inegi.org.mx/contenidos/saladeprensa/boletines/2024/ENVIPE/ENVIPE_24.pdf" target="_blank">INEGI</a>.</p>
                    </div>
                </div>
            </div>
        </div>

        <!-- Imagen 4: Comparación de dos imágenes -->
        <div class="col-md-12 mb-4">
            <div class="card h-100">
                <div class="row no-gutters">
                    <div class="col-md-6">
                        <img src="<?= base_url('images/tendencia_secuestros_peryear.png'); ?>" class="card-img img-thumbnail" alt="Imagen 4A" data-toggle="modal" data-target="#modalImagen4a">
                    </div>
                    <div class="col-md-6">
                        <img src="<?= base_url('images/tendencia_inegi.png'); ?>" class="card-img img-thumbnail" alt="Imagen 4B" data-toggle="modal" data-target="#modalImagen4b">
                    </div>
                </div>
                <div class="card-body">
                    <h5 class="card-title">Comparación</h5>
                    <p class="card-text">Ambas gráficas muestran la tendencia de secuestros en México. La verde es una proyección propia y la azul con gris, datos oficiales del INEGI, mostrando consistencia en picos y descensos.</p>
                </div>
            </div>
        </div>

        <!-- Modal Imagen 4A -->
        <div class="modal fade" id="modalImagen4a" tabindex="-1" aria-labelledby="modalLabel4a" aria-hidden="true">
            <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
                <div class="modal-content">
                    <div class="modal-header">
                        <h5 class="modal-title" id="modalLabel4a">Tendencia secuestros ENVIPE 2024</h5>
                        <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                            <span aria-hidden="true">&times;</span>
                        </button>
                    </div>
                    <div class="modal-body">
                        <img src="<?= base_url('images/tendencia_inegi2.png'); ?>" class="img-fluid" alt="Imagen 4A">
                        <p class="mt-3">La gráfica oficial del INEGI presenta las estimaciones de víctimas y delitos de secuestro en México entre 2016 y 2023. La línea azul claro corresponde a las víctimas, y la azul oscuro representa los delitos, con barras que muestran intervalos de confianza. Se aprecian picos en 2019 y una disminución posterior.</p>
                    </div>
                </div>
            </div>
        </div>
        <!-- Fin del Modal Imagen 4A -->

        <!-- Modal Imagen 4B -->
        <div class="modal fade" id="modalImagen4b" tabindex="-1" aria-labelledby="modalLabel4b" aria-hidden="true">
            <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
                <div class="modal-content">
                    <div class="modal-header">
                        <h5 class="modal-title" id="modalLabel4b">Tendencia promedio anual</h5>
                        <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                            <span aria-hidden="true">&times;</span>
                        </button>
                    </div>
                    <div class="modal-body">
                        <img src="<?= base_url('images/tendencia_promedio.png'); ?>" class="img-fluid" alt="Imagen 4B">
                        <p class="mt-3">Esta gráfica muestra la cantidad anual de secuestros proyectados en México entre 2016 y 2024. Los puntos verdes representan los valores calculados, conectados por una línea continua que destaca las variaciones anuales. La tendencia refleja un descenso inicial, seguido de un incremento sostenido hacia 2022 y una caída en 2024.</p>
                    </div>
                </div>
            </div>
        </div>
        <!-- Fin del Modal Imagen 4B -->
    </div>
    <!-- Fin del bloque "Tendencias" -->

    <!-- Nuevo Bloque -->
    <!-- Subencabezado para el nuevo bloque -->
    <h2 class="mt-5 mb-4">Resultados de la Transformación de Datos de Secuestros</h2>
    <div class="row">
        <!-- Aquí puedes agregar más imágenes siguiendo el mismo formato -->

        <!-- Imagen 5 -->
        <div class="col-md-4 mb-4">
            <div class="card h-100">
                <img src="<?= base_url('images/distribucion_por_estado.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 5" data-toggle="modal" data-target="#modalImagen5">
                <div class="card-body">
                    <h5 class="card-title">Distribución de Secuestros por Estado en México</h5>
                    <p class="card-text">Distribución de secuestros extraídos por estado en México.</p>
                </div>
            </div>
        </div>

        <!-- Modal Imagen 5 -->
        <div class="modal fade" id="modalImagen5" tabindex="-1" aria-labelledby="modalLabel5" aria-hidden="true">
            <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
                <div class="modal-content">
                    <div class="modal-header">
                        <h5 class="modal-title" id="modalLabel5">Distribución de Secuestros por Estado en México</h5>
                        <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                            <span aria-hidden="true">&times;</span>
                        </button>
                    </div>
                    <div class="modal-body">
                        <img src="<?= base_url('images/distribucion_por_estado.png'); ?>" class="img-fluid" alt="Imagen 5">
                        <p class="mt-3">La gráfica muestra la cantidad de secuestros extraídos por estado en México entre 2016 y octubre de 2024. El Estado de México tiene la mayor cantidad, seguido de Tamaulipas, mientras que estados como Baja California Sur y Campeche presentan las cifras más bajas.</p>
                    </div>
                </div>
            </div>
        </div>
        <!-- Fin del Modal Imagen 5 -->
         <!-- Imagen 6 -->
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/captor.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 6" data-toggle="modal" data-target="#modalImagen6">
        <div class="card-body">
            <h5 class="card-title">Distribución de Características del Captor en Secuestros</h5>
            <p class="card-text">Gráfica de distribución de tipos de captores en secuestros.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 6 -->
<div class="modal fade" id="modalImagen6" tabindex="-1" aria-labelledby="modalLabel6" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel6">Distribución de Características del Captor en Secuestros</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/captor.png'); ?>" class="img-fluid" alt="Imagen 6">
                <p class="mt-3">
    La gráfica representa la distribución de características asociadas a los captores en casos de secuestro. Los tipos de captores incluyen:  
    <br>- <strong>Persona común</strong>: Representa la categoría con mayor frecuencia, donde el captor no tiene vínculos con organizaciones o figuras relevantes.  
    <br>- <strong>Confianza</strong>: Implica situaciones en las que el captor se aprovecha de una relación cercana con la víctima.  
    <br>- <strong>Autoridad</strong>: Casos en los que se utilizan figuras de poder para realizar el secuestro.  
    <br>- <strong>Cártel</strong>: Involucra a grupos delictivos organizados como responsables del acto.  
    <br>- <strong>Complicidad</strong>: Participación de individuos en apoyo de otros para ejecutar el secuestro.  
    <br>- <strong>Suplantación de identidad</strong>: Uso de identidades falsas para engañar a la víctima y facilitar el secuestro.
</p>

            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 6 -->
<!-- Imagen 7 -->
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/captura.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 7" data-toggle="modal" data-target="#modalImagen7">
        <div class="card-body">
            <h5 class="card-title">Distribución por Tipo de Captura en Secuestros</h5>
            <p class="card-text">Gráfica de métodos de captura en secuestros.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 7 -->
<div class="modal fade" id="modalImagen7" tabindex="-1" aria-labelledby="modalLabel7" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel7">Distribución por Tipo de Captura en Secuestros</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/captura.png'); ?>" class="img-fluid" alt="Imagen 7">
                <p class="mt-3">
    La gráfica representa la distribución de los métodos de captura utilizados en secuestros. Los tipos incluyen:  
    <br>- <strong>No específico</strong>: Mayor frecuencia, cuando no se puede determinar un método claro.  
    <br>- <strong>Fuerza</strong>: Uso de violencia física para someter a la víctima.  
    <br>- <strong>Emboscada</strong>: Captura realizada mediante una estrategia sorpresa en un lugar premeditado.  
    <br>- <strong>Intimidación</strong>: Amenazas o coacción psicológica para lograr la captura.  
    <br>- <strong>Tecnológica</strong>: Uso de herramientas tecnológicas, como rastreo o monitoreo, para facilitar el secuestro.
</p>

            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 7 -->
<!-- Imagen 8 -->
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/distribucion_tipo_liberacion.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 8" data-toggle="modal" data-target="#modalImagen8">
        <div class="card-body">
            <h5 class="card-title">Distribución por Tipo de Liberación en Secuestros</h5>
            <p class="card-text">Gráfica de distribución de los tipos de liberación en secuestros, incluyendo operativos, negociaciones y casos no clasificados.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 8 -->
<div class="modal fade" id="modalImagen8" tabindex="-1" aria-labelledby="modalLabel8" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel8">Distribución por Tipo de Liberación en Secuestros</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/distribucion_tipo_liberacion.png'); ?>" class="img-fluid" alt="Imagen 8">
                <p class="mt-3">
    La gráfica muestra la distribución de los tipos de liberación en secuestros:  
    <br>- <strong>Liberación en operativo</strong>: Casos donde las víctimas fueron liberadas mediante acciones de las autoridades.  
    <br>- <strong>Liberación general</strong>: Incluye liberaciones que no se clasifican en otras categorías, como el escape por parte de la víctima.  
    <br>- <strong>Liberación por negociación</strong>: Liberaciones obtenidas mediante acuerdos o pagos.  
    <br>- <strong>No clasificado</strong>: Casos donde no existió liberación o no se encontró información sobre la misma.  
</p>

            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 8 -->
<!-- Imagen 9 -->
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/lugar.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 9" data-toggle="modal" data-target="#modalImagen9">
        <div class="card-body">
            <h5 class="card-title">Distribución por Lugar de Secuestro</h5>
            <p class="card-text">Gráfica de lugares de secuestro, destacando casa, transporte y vía pública.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 9 -->
<div class="modal fade" id="modalImagen9" tabindex="-1" aria-labelledby="modalLabel9" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel9">Distribución por Lugar de Secuestro</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/lugar.png'); ?>" class="img-fluid" alt="Imagen 9">
                <p class="mt-3">
                    La gráfica muestra la distribución de lugares donde ocurrieron secuestros:  
                    <br>- <strong>Casa</strong>: Casos donde la víctima fue secuestrada en su hogar o en las cercanías del mismo.  
                    <br>- <strong>Transporte</strong>: Incluye secuestros ocurridos en transporte público o personal, siendo detenido en tránsito.  
                    <br>- <strong>Vía pública</strong>: Corresponde a secuestros realizados en espacios públicos o en situaciones que no encajan en las categorías anteriores.  
                </p>
            </div>
        </div>
    </div>
</div>

<!-- Imagen 13 -->
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/tipo_secuestro.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 13" data-toggle="modal" data-target="#modalImagen13">
        <div class="card-body">
            <h5 class="card-title">Distribución por Tipo de Secuestro</h5>
            <p class="card-text">Gráfica que muestra la distribución de los diferentes tipos de secuestro, como extorsivo, general, de menores y otros.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 13 -->
<div class="modal fade" id="modalImagen13" tabindex="-1" aria-labelledby="modalLabel13" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel13">Distribución por Tipo de Secuestro</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/tipo_secuestro.png'); ?>" class="img-fluid" alt="Imagen 13">
                <p class="mt-3">
                    La gráfica muestra la distribución de los distintos tipos de secuestro registrados:  
                    <br>- <strong>Secuestro extorsivo</strong>: Casos donde el objetivo principal es obtener dinero o bienes a cambio de la liberación de la víctima.  
                    <br>- <strong>Secuestro general</strong>: Incluye secuestros que no se clasifican en categorías específicas o donde no se detalla el propósito.  
                    <br>- <strong>Secuestro con fines de explotación sexual</strong>: Casos donde las víctimas son secuestradas con el objetivo de explotación sexual.  
                    <br>- <strong>Secuestro de menores</strong>: Involucra la privación de libertad de niños o adolescentes.  
                    <br>- <strong>Secuestro por delincuencia organizada</strong>: Casos en los que grupos criminales organizados están involucrados.  
                    <br>- <strong>Secuestro virtual</strong>: No hay privación física, pero se hace creer a las familias que un ser querido está secuestrado para obtener dinero.  
                    <br>- <strong>Secuestro familiar</strong>: Realizado dentro del entorno familiar, generalmente relacionado con disputas personales o legales.  
                    <br>- <strong>Secuestro exprés</strong>: Privación de libertad por un corto periodo para obtener dinero u otros bienes rápidamente.  
                </p>
            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 13 -->




        <!-- Continúa agregando más imágenes aquí -->
    </div>

    <h2 class="mt-5 mb-4">Análisis de Campos Relacionables</h2>
    <div class="row">
 
<!-- Imagen 10 -->
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/hotmap_estado_captor.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 10" data-toggle="modal" data-target="#modalImagen10">
        <div class="card-body">
            <h5 class="card-title">Mapa de Calor de Secuestros por Estado y Tipo de Captor</h5>
            <p class="card-text">Mapa de calor que muestra la relación entre estados, tipos de captores y la cantidad de secuestros, con tonos que indican la intensidad.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 10 -->
<div class="modal fade" id="modalImagen10" tabindex="-1" aria-labelledby="modalLabel10" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel10">Mapa de Calor de Secuestros por Estado y Tipo de Captor</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/hotmap_estado_captor.png'); ?>" class="img-fluid" alt="Imagen 10">
                <p class="mt-3">
                    La gráfica es un mapa de calor que relaciona la cantidad de secuestros con el estado y el tipo de captor.  
                    <br>Cada fila representa un estado, y cada columna corresponde a un tipo de captor, como autoridad, cártel, complicidad, confianza, persona común o suplantación de identidad.  
                    <br>Los tonos de color indican la intensidad: un azul más oscuro representa una mayor cantidad de secuestros, mientras que los tonos más claros indican menor incidencia.  
                    <br>Esto permite identificar combinaciones destacadas de estado y tipo de captor.
                </p>
            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 10 -->
<!-- Imagen 11 -->
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/hotmap_total_estado_anio.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 11" data-toggle="modal" data-target="#modalImagen11">
        <div class="card-body">
            <h5 class="card-title">Total de Secuestros por Estado y Año</h5>
            <p class="card-text">Mapa de calor que muestra el total de secuestros por estado y año, con colores que indican la intensidad.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 11 -->
<div class="modal fade" id="modalImagen11" tabindex="-1" aria-labelledby="modalLabel11" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel11">Total de Secuestros por Estado y Año</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/hotmap_total_estado_anio.png'); ?>" class="img-fluid" alt="Imagen 11">
                <p class="mt-3">
                    La gráfica es un mapa de calor que representa el total de secuestros por estado y año.  
                    <br>Cada fila corresponde a un estado, y cada columna representa un año.  
                    <br>Los tonos de color varían desde el blanco (menor cantidad de secuestros) hasta el rojo oscuro (mayor cantidad de secuestros).  
                    <br>Esto permite identificar estados con alta incidencia en periodos específicos, resaltando patrones temporales y regionales en el delito.
                </p>
            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 11 -->

<!-- Imagen 12 -->
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/liberacion_anual.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 12" data-toggle="modal" data-target="#modalImagen12">
        <div class="card-body">
            <h5 class="card-title">Porcentaje de Secuestros con y sin Liberación por Año</h5>
            <p class="card-text">Diagrama de barras apiladas que muestra el porcentaje de secuestros con liberación (azul) y sin liberación o no clasificados (rojo) por año.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 12 -->
<div class="modal fade" id="modalImagen12" tabindex="-1" aria-labelledby="modalLabel12" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel12">Porcentaje de Secuestros con y sin Liberación por Año</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/liberacion_anual.png'); ?>" class="img-fluid" alt="Imagen 12">
                <p class="mt-3">
                    La gráfica es un diagrama de barras apiladas que muestra el porcentaje de secuestros con y sin liberación por año.  
                    <br>- La barra azul indica los casos donde hubo liberación.  
                    <br>- La barra roja representa los casos sin liberación, que también incluye aquellos clasificados como "No clasificado", lo que implica que no se encontró información al respecto.  
                    <br>El gráfico permite analizar la proporción de secuestros con resultados positivos (liberación) frente a aquellos sin información suficiente o sin liberación, destacando tendencias anuales en ambos casos.
                </p>
            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 12 -->

<!-- Imagen 14 -->
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/proporcion_tipo_lugar.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 14" data-toggle="modal" data-target="#modalImagen14">
        <div class="card-body">
            <h5 class="card-title">Proporción de Tipos de Secuestro según Lugar</h5>
            <p class="card-text">Gráfica de barras apiladas que muestra la proporción de tipos de secuestro en diferentes lugares: vía pública, casa y transporte.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 14 -->
<div class="modal fade" id="modalImagen14" tabindex="-1" aria-labelledby="modalLabel14" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel14">Proporción de Tipos de Secuestro según Lugar</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/proporcion_tipo_lugar.png'); ?>" class="img-fluid" alt="Imagen 14">
                <p class="mt-3">
                    La gráfica representa un <strong>diagrama de barras apiladas normalizado</strong>, donde cada barra corresponde a un lugar de secuestro (vía pública, casa y transporte) y está dividida por colores según los diferentes tipos de secuestro.  
                    <br>- <strong>Lectura</strong>: Cada barra muestra el porcentaje relativo de cada tipo de secuestro dentro del lugar correspondiente. Esto permite comparar cómo varían los tipos de secuestro según el lugar.  
                    <br>Por ejemplo:  
                    <br> - En la <strong>vía pública</strong>, predominan los secuestros extorsivos y generales.  
                    <br> - En <strong>casas</strong>, se observa una proporción similar, pero con una ligera diferencia en las categorías menores.  
                    <br> - En <strong>transporte</strong>, los secuestros extorsivos también son predominantes.  
                    <br>- <strong>Ejes</strong>:  
                    <br>  - El eje vertical indica la <strong>proporción de secuestros</strong> (de 0 a 1, representando porcentajes).  
                    <br>  - El eje horizontal categoriza los lugares: vía pública, casa y transporte.  
                    <br>Esta visualización permite analizar cómo se distribuyen los tipos de secuestro en función del lugar, resaltando patrones específicos para cada ubicación.
                </p>
            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 14 -->
<!-- Imagen 15 -->
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/relacion_captor_captura.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 15" data-toggle="modal" data-target="#modalImagen15">
        <div class="card-body">
            <h5 class="card-title">Relación entre Tipo de Captor y Método de Captura</h5>
            <p class="card-text">Gráfica de barras agrupadas que muestra la relación entre los tipos de captores y los métodos de captura registrados.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 15 -->
<div class="modal fade" id="modalImagen15" tabindex="-1" aria-labelledby="modalLabel15" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel15">Relación entre Tipo de Captor y Método de Captura</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/relacion_captor_captura.png'); ?>" class="img-fluid" alt="Imagen 15">
                <p class="mt-3">
                    La gráfica presenta la cantidad de secuestros clasificados según el tipo de captor (autoridad, cártel, complicidad, confianza, persona común y suplantación de identidad) y el método de captura empleado (emboscada, fuerza, intimidación, no específico y tecnológica). Cada barra agrupada está segmentada por colores que representan los diferentes métodos de captura. Los datos permiten observar la distribución y frecuencia de estas combinaciones en el conjunto de registros analizados.
                </p>
            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 15 -->
<!-- Imagen 16 -->
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/relacion_estado_tipo_casa.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 16" data-toggle="modal" data-target="#modalImagen16">
        <div class="card-body">
            <h5 class="card-title">Relación entre Estado y Tipo de Secuestro en Casa</h5>
            <p class="card-text">Relación de los diferentes tipos de secuestro realizados en casas o zonas cercanas según los estados de México.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 16 -->
<div class="modal fade" id="modalImagen16" tabindex="-1" aria-labelledby="modalLabel16" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel16">Relación entre Estado y Tipo de Secuestro en Casa</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/relacion_estado_tipo_casa.png'); ?>" class="img-fluid" alt="Imagen 16">
                <p class="mt-3">
                    La gráfica de barras agrupadas muestra la cantidad de secuestros registrados en los estados de México según su clasificación por tipo. 
                    Cada barra representa un tipo de secuestro, como <strong>secuestro exprés</strong>, <strong>extorsivo</strong>, <strong>familiar</strong>, <strong>de menores</strong>, <strong>virtual</strong>, <strong>por delincuencia organizada</strong>, 
                    <strong>con fines de explotación sexual</strong> y <strong>general</strong>. Los estados con mayor incidencia son el Estado de México, Veracruz y Tamaulipas.
                </p>
            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 16 -->
<!-- Imagen 17 -->
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/relacion_estado_tipo_publica.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 17" data-toggle="modal" data-target="#modalImagen17">
        <div class="card-body">
            <h5 class="card-title">Relación entre Estado y Tipo de Secuestro en Vía Pública</h5>
            <p class="card-text">Relación de los tipos de secuestro en vía pública según los estados de México, destacando el Estado de México y Veracruz.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 17 -->
<div class="modal fade" id="modalImagen17" tabindex="-1" aria-labelledby="modalLabel17" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel17">Relación entre Estado y Tipo de Secuestro en Vía Pública</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/relacion_estado_tipo_publica.png'); ?>" class="img-fluid" alt="Imagen 17">
                <p class="mt-3">
                    La gráfica muestra la cantidad de secuestros ocurridos en vía pública según los estados de México. Las barras están categorizadas por tipo de secuestro, como <strong>extorsivo</strong>, <strong>general</strong>, <strong>familiar</strong>, y otros. Se observa que el Estado de México y Veracruz tienen la mayor incidencia, destacándose en los tipos de secuestro general y extorsivo.
                </p>
            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 17 -->
<!-- Imagen 18 -->
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/relacion_estado_tipo_transporte.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 18" data-toggle="modal" data-target="#modalImagen18">
        <div class="card-body">
            <h5 class="card-title">Relación entre Estado y Tipo de Secuestro en Transporte</h5>
            <p class="card-text">Relación de los tipos de secuestro en transporte según los estados de México, destacando el Estado de México y Guanajuato.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 18 -->
<div class="modal fade" id="modalImagen18" tabindex="-1" aria-labelledby="modalLabel18" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel18">Relación entre Estado y Tipo de Secuestro en Transporte</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/relacion_estado_tipo_transporte.png'); ?>" class="img-fluid" alt="Imagen 18">
                <p class="mt-3">
                    La gráfica ilustra la cantidad de secuestros ocurridos en transporte clasificados por estado en México y categorizados según el tipo de secuestro. Los tipos incluyen <strong>secuestro extorsivo</strong>, <strong>secuestro general</strong>, <strong>secuestro exprés</strong>, entre otros. Los estados con mayor incidencia son el Estado de México y Guanajuato, especialmente en los secuestros extorsivos y generales. La información está representada mediante barras diferenciadas por colores para facilitar la comparación entre estados y tipos de secuestro.
                </p>
            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 18 -->
<!-- Imagen 19 -->
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/relacion_lugar_captor_tipo.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 19" data-toggle="modal" data-target="#modalImagen19">
        <div class="card-body">
            <h5 class="card-title">Relación entre Lugar, Captor y Tipo de Captura</h5>
            <p class="card-text">Proporción de tipos de captura según el lugar del secuestro y el perfil del captor.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 19 -->
<div class="modal fade" id="modalImagen19" tabindex="-1" aria-labelledby="modalLabel19" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel19">Relación entre Lugar, Captor y Tipo de Captura</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/relacion_lugar_captor_tipo.png'); ?>" class="img-fluid" alt="Imagen 19">
                <p class="mt-3">
                    <strong>Descripción breve:</strong>  
                    La gráfica detalla la proporción de tipos de captura según el lugar del secuestro y el perfil del captor, mostrando variaciones significativas entre casa, transporte y vía pública.  
                    <br><br>
                    <strong>Descripción detallada:</strong>  
                    Esta gráfica compara tres dimensiones clave en los secuestros:  
                    - <strong>Lugar del secuestro:</strong> casa, transporte, vía pública.  
                    - <strong>Tipo de captor:</strong> autoridad, cártel, complicidad, confianza, persona común, suplantación de identidad.  
                    - <strong>Tipo de captura:</strong> emboscada, fuerza, intimidación, no específico, tecnológica.  
                    <br>
                    Cada barra representa una combinación de lugar y captor, y los colores en cada barra muestran las proporciones de los diferentes tipos de captura.  
                    <br><br>
                    <strong>Cómo interpretarla:</strong>  
                    - <strong>Eje horizontal:</strong> Muestra los tipos de captor agrupados por lugar del secuestro (casa, transporte, vía pública).  
                    - <strong>Eje vertical:</strong> Representa la proporción total de secuestros (100%), que se divide entre los tipos de captura (colores).  
                    - <strong>Colores:</strong> Cada color indica un tipo de captura.  
                </p>
            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 19 -->

        <!-- Continúa agregando más imágenes aquí -->
    </div>

    <h2 class="mt-5 mb-4">Tipos de Secuestro a lo Largo del Tiempo en los 6 Estados con Mayor Incidencia</h2>
    <div class="row">
 
<!-- Imagen 20 -->
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/secuestro_chiapas.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 20" data-toggle="modal" data-target="#modalImagen20">
        <div class="card-body">
            <h5 class="card-title">Distribución de Tipos de Secuestro por Año en Chiapas</h5>
            <p class="card-text">Visualización de los tipos de secuestro extraídos en Chiapas, clasificados por año y categoría entre 2016 y 2024.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 20 -->
<div class="modal fade" id="modalImagen20" tabindex="-1" aria-labelledby="modalLabel20" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel20">Distribución de Tipos de Secuestro por Año en Chiapas</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/secuestro_chiapas.png'); ?>" class="img-fluid" alt="Imagen 20">
                <p class="mt-3">
                    La gráfica presenta la cantidad de secuestros extraídos en Chiapas clasificados por tipo y año entre 2016 y 2024. Cada barra indica el número de casos registrados en un año específico, diferenciados por categorías:  
                    <br>- <strong>Secuestro con fines de explotación sexual</strong>: Marcado en color naranja.  
                    <br>- <strong>Secuestro de menores</strong>: En amarillo.  
                    <br>- <strong>Secuestro extorsivo</strong>: Representado en verde, destacando como una de las categorías con mayor incidencia en varios años.  
                    <br>- <strong>Secuestro familiar</strong>: En azul claro.  
                    <br>- <strong>Secuestro general</strong>: Azul más oscuro.  
                    <br>- <strong>Secuestro por delincuencia organizada</strong>: En morado.  
                    <br>- <strong>Secuestro virtual</strong>: En rosa.  
                    La visualización nos permite observar tendencias y variaciones anuales en cada categoría de secuestro, facilitando el análisis del comportamiento en esta región.
                </p>
            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 20 -->
<!-- Imagen 21 -->
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/secuestro_edomex.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 21" data-toggle="modal" data-target="#modalImagen21">
        <div class="card-body">
            <h5 class="card-title">Distribución de Tipos de Secuestro por Año en el Estado de México</h5>
            <p class="card-text">Distribución anual de los tipos de secuestro registrados en el Estado de México, desde 2016 hasta 2024.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 21 -->
<div class="modal fade" id="modalImagen21" tabindex="-1" aria-labelledby="modalLabel21" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel21">Distribución de Tipos de Secuestro por Año en el Estado de México</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/secuestro_edomex.png'); ?>" class="img-fluid" alt="Imagen 21">
                <p class="mt-3">
                    Esta gráfica de barras apiladas ilustra la cantidad de secuestros en el Estado de México a lo largo de los años 2016 a 2024, diferenciados según el tipo de secuestro. Cada barra representa la suma de casos por año y está dividida por categorías de secuestro como extorsivo, general, familiar, entre otros.
                    <br><br>
                    Esto nos permite identificar tendencias clave, como el predominio del <strong>secuestro extorsivo</strong> en varios años y los cambios en la incidencia de otras categorías a lo largo del tiempo. Esta visualización ayuda a comprender cómo ciertos tipos de secuestro han evolucionado en el Estado de México.
                </p>
            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 21 -->
<!-- Imagen 22 -->
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/secuestro_gerrero.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 22" data-toggle="modal" data-target="#modalImagen22">
        <div class="card-body">
            <h5 class="card-title">Distribución de Secuestros por Año en Guerrero</h5>
            <p class="card-text">Distribución anual de secuestros en Guerrero, mostrando las tendencias desde 2016 hasta 2024.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 22 -->
<div class="modal fade" id="modalImagen22" tabindex="-1" aria-labelledby="modalLabel22" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel22">Distribución de Secuestros por Año en Guerrero</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/secuestro_gerrero.png'); ?>" class="img-fluid" alt="Imagen 22">
                <p class="mt-3">
                    La gráfica de barras segmentadas ilustra cómo se han distribuido los secuestros anualmente en Guerrero entre 2016 y 2024. 
                    Esto nos permite observar cambios y tendencias a lo largo del tiempo, ayudándonos a identificar patrones relevantes para el análisis de este delito en la región.
                </p>
            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 22 -->
 <!-- Imagen 23 --> 
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/secuestro_mexico_city.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 23" data-toggle="modal" data-target="#modalImagen23">
        <div class="card-body">
            <h5 class="card-title">Distribución de Secuestros por Año en Ciudad de México</h5>
            <p class="card-text">Visualización de la distribución anual de secuestros registrados en Ciudad de México.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 23 -->
<div class="modal fade" id="modalImagen23" tabindex="-1" aria-labelledby="modalLabel23" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel23">Distribución de Secuestros por Año en Ciudad de México</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/secuestro_mexico_city.png'); ?>" class="img-fluid" alt="Imagen 23">
                <p class="mt-3">La gráfica ilustra la cantidad de secuestros registrados anualmente en Ciudad de México. Esto nos permite identificar patrones en la ocurrencia de secuestros a lo largo de los años y detectar posibles tendencias o cambios significativos en la dinámica de este delito en la región.</p>
            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 23 -->
<!-- Imagen 24 --> 
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/secuestro_mich.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 24" data-toggle="modal" data-target="#modalImagen24">
        <div class="card-body">
            <h5 class="card-title">Distribución de Secuestros por Año en Michoacán</h5>
            <p class="card-text">Análisis anual de los secuestros registrados en Michoacán, mostrando las tendencias y variaciones.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 24 -->
<div class="modal fade" id="modalImagen24" tabindex="-1" aria-labelledby="modalLabel24" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel24">Distribución de Secuestros por Año en Michoacán</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/secuestro_mich.png'); ?>" class="img-fluid" alt="Imagen 24">
                <p class="mt-3">
                    Esta gráfica representa la distribución de secuestros en Michoacán a lo largo de los años, permitiéndonos observar cómo han variado los registros anuales. Las barras muestran los registros específicos de cada año, lo que nos ayuda a identificar periodos con mayor incidencia de secuestros y posibles reducciones en los mismos. Esto nos permite analizar las tendencias y la evolución del problema en este estado.
                </p>
            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 24 -->
        <!-- Imagen 26 --> 
        <div class="col-md-4 mb-4">
            <div class="card h-100">
                <img src="<?= base_url('images/secuestro_tama.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 26" data-toggle="modal" data-target="#modalImagen26">
                <div class="card-body">
                    <h5 class="card-title">Distribución de Secuestros por Año en Tamaulipas</h5>
                    <p class="card-text">Tendencias anuales de secuestros registrados en Tamaulipas, mostrando variaciones significativas entre los años.</p>
                </div>
            </div>
        </div>

        <!-- Modal Imagen 26 -->
        <div class="modal fade" id="modalImagen26" tabindex="-1" aria-labelledby="modalLabel26" aria-hidden="true">
            <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
                <div class="modal-content">
                    <div class="modal-header">
                        <h5 class="modal-title" id="modalLabel26">Distribución de Secuestros por Año en Tamaulipas</h5>
                        <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                            <span aria-hidden="true">&times;</span>
                        </button>
                    </div>
                    <div class="modal-body">
                        <img src="<?= base_url('images/secuestro_tama.png'); ?>" class="img-fluid" alt="Imagen 26">
                        <p class="mt-3">
                            Esta gráfica ilustra la evolución de los secuestros en Tamaulipas a lo largo de los años, destacando las fluctuaciones en el número de incidentes reportados. Cada barra representa el total de casos registrados en un año específico, categorizados por tipos de secuestro. La visualización permite observar picos notables, como el incremento en 2023, así como periodos de menor actividad. Esto nos ayuda a identificar patrones y entender cómo ha cambiado la incidencia de secuestros en este estado a través del tiempo.
                        </p>
                    </div>
                </div>
            </div>
        </div>
        <!-- Fin del Modal Imagen 26 -->


        <!-- Continúa agregando más imágenes aquí -->
    </div>


<h2 class="mt-5 mb-4">Los 15 Municipios y Ciudades con mayor incidencias</h2>
<div class="row">

        <!-- Imagen 25 --> 
        <div class="col-md-4 mb-4">
            <div class="card h-100">
                <img src="<?= base_url('images/dsitribucion_municipios_mayor.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 25" data-toggle="modal" data-target="#modalImagen25">
                <div class="card-body">
                    <h5 class="card-title">Distribución de Tipos de Secuestro en los 15 Municipios con Mayor Incidencia</h5>
                    <p class="card-text">Análisis visual de los 15 municipios con mayor incidencia de secuestros en México, clasificando los casos según su tipo.</p>
                </div>
            </div>
        </div>

        <!-- Modal Imagen 25 -->
        <div class="modal fade" id="modalImagen25" tabindex="-1" aria-labelledby="modalLabel25" aria-hidden="true">
            <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
                <div class="modal-content">
                    <div class="modal-header">
                        <h5 class="modal-title" id="modalLabel25">Distribución de Tipos de Secuestro en los 15 Municipios con Mayor Incidencia</h5>
                        <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                            <span aria-hidden="true">&times;</span>
                        </button>
                    </div>
                    <div class="modal-body">
                        <img src="<?= base_url('images/dsitribucion_municipios_mayor.png'); ?>" class="img-fluid" alt="Imagen 25">
                        <p class="mt-3">Esta gráfica muestra la distribución de secuestros clasificados por tipo en los 15 municipios con mayor incidencia de casos en México. En el eje vertical se encuentran los nombres de los municipios, mientras que el eje horizontal representa la cantidad total de secuestros extraídos. Los colores de las barras indican los diferentes tipos de secuestros registrados, permitiendo una visualización clara de las dinámicas en cada región. Municipios como Toluca, Monterrey y Ecatepec de Morelos destacan por su alta incidencia, mientras que otros como Pachuca de Soto y Madero tienen cifras más reducidas.</p>
                    </div>
                </div>
            </div>
        </div>
        <!-- Fin del Modal Imagen 25 -->
<!-- Imagen 27 --> 
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/secuestros_chil_gua_nauc.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 27" data-toggle="modal" data-target="#modalImagen27">
        <div class="card-body">
            <h5 class="card-title">Secuestros por Año en: Chilpancingo de los Bravo, Guadalajara, Naucalpan de Juárez</h5>
            <p class="card-text">Este gráfico muestra la cantidad de secuestros registrados anualmente en tres municipios, evidenciando tendencias únicas en cada región.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 27 -->
<div class="modal fade" id="modalImagen27" tabindex="-1" aria-labelledby="modalLabel27" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel27">Secuestros por Año en: Chilpancingo de los Bravo, Guadalajara, Naucalpan de Juárez</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/secuestros_chil_gua_nauc.png'); ?>" class="img-fluid" alt="Imagen 27">
                <p class="mt-3">
                    El gráfico está dividido en tres paneles, uno para cada municipio: Chilpancingo de los Bravo, Guadalajara y Naucalpan de Juárez. En el eje vertical se encuentra la cantidad de secuestros registrados, mientras que en el eje horizontal se representan los años de 2016 a 2024.
                    <br><br>
                    Cada barra en el gráfico está codificada por color para identificar los años correspondientes, facilitando una comparación visual entre periodos. Chilpancingo de los Bravo muestra fluctuaciones destacadas, con picos notables en los años recientes. Guadalajara, por su parte, tiene un aumento evidente en el año 2022, mientras que Naucalpan de Juárez presenta un comportamiento similar al observar repuntes en 2022 y 2024.
                </p>
            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 27 -->

<!-- Imagen 28 --> 
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/secuestros_izt_rey_ecat.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 28" data-toggle="modal" data-target="#modalImagen28">
        <div class="card-body">
            <h5 class="card-title">Secuestros por Año en: Iztapalapa, Reynosa, Ecatepec de Morelos</h5>
            <p class="card-text">Evolución anual de los secuestros en los municipios de Iztapalapa, Reynosa y Ecatepec de Morelos entre 2016 y 2024.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 28 -->
<div class="modal fade" id="modalImagen28" tabindex="-1" aria-labelledby="modalLabel28" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel28">Secuestros por Año en: Iztapalapa, Reynosa, Ecatepec de Morelos</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/secuestros_izt_rey_ecat.png'); ?>" class="img-fluid" alt="Imagen 28">
                <p class="mt-3">
                    El gráfico presenta la evolución anual de los secuestros en los municipios de Iztapalapa, Reynosa y Ecatepec de Morelos. Cada panel corresponde a un municipio y muestra las cifras de secuestros anuales mediante barras codificadas por colores, donde cada color representa un año entre 2016 y 2024. 
                    <br><strong>Iztapalapa:</strong> Se destaca un pico en 2017 y fluctuaciones importantes hasta 2022. <br>
                    <strong>Reynosa:</strong> Registra un aumento significativo en 2024 después de cifras más bajas en años anteriores. <br>
                    <strong>Ecatepec de Morelos:</strong> Muestra un repunte en 2020, con una disminución gradual hacia 2024.
                </p>
            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 28 -->
<!-- Imagen 29 -->
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/secuestros_tol_cuer_mont.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 29" data-toggle="modal" data-target="#modalImagen29">
        <div class="card-body">
            <h5 class="card-title">Secuestros por Año en: Toluca, Cuernavaca, Monterrey</h5>
            <p class="card-text">Análisis de la evolución de secuestros registrados anualmente en Toluca, Cuernavaca y Monterrey, destacando los años con mayores incidencias.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 29 -->
<div class="modal fade" id="modalImagen29" tabindex="-1" aria-labelledby="modalLabel29" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel29">Secuestros por Año en: Toluca, Cuernavaca, Monterrey</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/secuestros_tol_cuer_mont.png'); ?>" class="img-fluid" alt="Imagen 29">
                <p class="mt-3">
                    Esta gráfica compara la cantidad de secuestros extraídos anualmente entre 2016 y 2024 en tres municipios: Toluca, Cuernavaca y Monterrey. Cada panel representa un municipio diferente, mostrando el número de casos por año mediante barras codificadas por colores según el año.<br>
                    - <strong>Cuernavaca</strong> muestra un incremento significativo en 2017 y 2024, mientras que los demás años presentan cifras moderadas.<br>
                    - <strong>Monterrey</strong> evidencia un crecimiento constante desde 2017, alcanzando picos en 2023 y 2024.<br>
                    - <strong>Toluca</strong> destaca con un aumento marcado en 2022, siendo este año el de mayor incidencia en el período analizado.
                </p>
            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 29 -->
<!-- Imagen 30 -->
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/secuestros_tlal_mat_mor.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 30" data-toggle="modal" data-target="#modalImagen30">
        <div class="card-body">
            <h5 class="card-title">Secuestros por Año en: Tlalpan, Matamoros, Morelia</h5>
            <p class="card-text">Análisis de la distribución anual de secuestros en Tlalpan, Matamoros y Morelia, destacando las variaciones en la incidencia a lo largo de los años.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 30 -->
<div class="modal fade" id="modalImagen30" tabindex="-1" aria-labelledby="modalLabel30" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel30">Secuestros por Año en: Tlalpan, Matamoros, Morelia</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/secuestros_tlal_mat_mor.png'); ?>" class="img-fluid" alt="Imagen 30">
                <p class="mt-3">Esta gráfica muestra la cantidad de secuestros reportados entre 2016 y 2024 en tres municipios: Tlalpan, Matamoros y Morelia. Cada panel representa un municipio distinto y destaca las tendencias anuales en el número de casos:</p>
                <ul>
                    <li><strong>Matamoros:</strong> Presenta un aumento significativo en 2023, siendo este el año con mayor incidencia en el período analizado.</li>
                    <li><strong>Morelia:</strong> Evidencia un comportamiento consistente con mayores registros entre 2016 y 2019, disminuyendo en años posteriores.</li>
                    <li><strong>Tlalpan:</strong> Destaca en 2019 como el año con mayor número de casos, con una tendencia estable en otros años.</li>
                </ul>
            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 30 -->
<!-- Imagen 31 --> 
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/secuestros_tux_pach_mad.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 31" data-toggle="modal" data-target="#modalImagen31">
        <div class="card-body">
            <h5 class="card-title">Secuestros por Año en: Tuxtla Gutiérrez, Madero, Pachuca de Soto</h5>
            <p class="card-text">Tendencias de secuestros reportados anualmente en los municipios de Tuxtla Gutiérrez, Madero y Pachuca de Soto.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 31 -->
<div class="modal fade" id="modalImagen31" tabindex="-1" aria-labelledby="modalLabel31" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel31">Secuestros por Año en: Tuxtla Gutiérrez, Madero, Pachuca de Soto</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/secuestros_tux_pach_mad.png'); ?>" class="img-fluid" alt="Imagen 31">
                <p class="mt-3">
                    La gráfica compara la incidencia anual de secuestros en tres municipios clave: 
                    <ul>
                        <li><b>Madero:</b> Presenta un incremento significativo en 2019, mientras que en otros años la tendencia es más estable.</li>
                        <li><b>Pachuca de Soto:</b> Resalta con un incremento notable en 2020 y 2024, mostrando picos significativos en estos años.</li>
                        <li><b>Tuxtla Gutiérrez:</b> Exhibe un crecimiento constante hacia 2024, que se posiciona como el año con más casos reportados.</li>
                    </ul>
                </p>
            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 31 -->



    <!-- Continúa agregando más imágenes aquí -->
</div>


<h2 class="mt-5 mb-4">Distribución Geográfica de Secuestros en México por año</h2>
<div class="row">

        <!-- Imagen 32 --> 
        <div class="col-md-4 mb-4">
            <div class="card h-100">
                <img src="<?= base_url('images/mexico_2019.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 32" data-toggle="modal" data-target="#modalImagen32">
                <div class="card-body">
                    <h5 class="card-title">Distribución Geográfica de Secuestros en México en 2019</h5>
                    <p class="card-text">Mapa que muestra la concentración de secuestros registrados en los diferentes estados de México durante el año 2019.</p>
                </div>
            </div>
        </div>

        <!-- Modal Imagen 32 -->
        <div class="modal fade" id="modalImagen32" tabindex="-1" aria-labelledby="modalLabel32" aria-hidden="true">
            <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
                <div class="modal-content">
                    <div class="modal-header">
                        <h5 class="modal-title" id="modalLabel32">Distribución Geográfica de Secuestros en México en 2019</h5>
                        <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                            <span aria-hidden="true">&times;</span>
                        </button>
                    </div>
                    <div class="modal-body">
                        <img src="<?= base_url('images/mexico_2019.png'); ?>" class="img-fluid" alt="Imagen 32">
                        <p class="mt-3">
                            Este mapa ilustra la distribución geográfica de los secuestros reportados en México durante el año 2019, utilizando un esquema de colores que refleja la densidad de casos por estado. Los tonos más oscuros indican una mayor cantidad de secuestros, mientras que los tonos más claros representan una menor incidencia.
                            <br><br>
                            <strong>Estado de México</strong> aparece como el estado con mayor número de secuestros, destacado en un rojo intenso. Otros estados como Veracruz y Puebla muestran niveles moderadamente altos, representados en tonos anaranjados. Gran parte de los estados del norte y sur del país presentan una incidencia baja, reflejada en tonos amarillos.
                        </p>
                    </div>
                </div>
            </div>
        </div>
        <!-- Fin del Modal Imagen 32 -->
<!-- Imagen 33 --> 
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/mexico_2020.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 33" data-toggle="modal" data-target="#modalImagen33">
        <div class="card-body">
            <h5 class="card-title">Distribución Geográfica de Secuestros en México en 2020</h5>
            <p class="card-text">Mapa que ilustra la distribución de secuestros en los estados de México durante 2020, destacando las zonas con mayor y menor incidencia.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 33 -->
<div class="modal fade" id="modalImagen33" tabindex="-1" aria-labelledby="modalLabel33" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel33">Distribución Geográfica de Secuestros en México en 2020</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/mexico_2020.png'); ?>" class="img-fluid" alt="Imagen 33">
                <p class="mt-3">Este mapa presenta la distribución geográfica de los secuestros reportados en México durante el año 2020. Los estados están coloreados en tonos que van del verde al amarillo, donde el verde representa la mayor cantidad de secuestros y el amarillo indica menor incidencia.</p>
                <p>Se observa que los secuestros estuvieron más concentrados en algunos estados del centro del país, como el Estado de México, que se encuentra destacado en tonos verdes, lo que denota una mayor incidencia en comparación con otros estados. Mientras tanto, varias regiones del norte y sur del país presentan niveles más bajos, reflejados en tonos amarillos.</p>
            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 33 -->

<!-- Imagen 34 -->
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/mexico_2021.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 34" data-toggle="modal" data-target="#modalImagen34">
        <div class="card-body">
            <h5 class="card-title">Distribución Geográfica de Secuestros en México en 2021</h5>
            <p class="card-text">Mapa que muestra la distribución de secuestros en México durante 2021, destacando los estados con mayor y menor incidencia mediante tonos de color rojo.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 34 -->
<div class="modal fade" id="modalImagen34" tabindex="-1" aria-labelledby="modalLabel34" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel34">Distribución Geográfica de Secuestros en México en 2021</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/mexico_2021.png'); ?>" class="img-fluid" alt="Imagen 34">
                <p class="mt-3">Este mapa ilustra la distribución geográfica de los secuestros reportados en México durante 2021. Los colores en el mapa van de tonos claros a oscuros en una escala de rojo, donde los tonos más oscuros representan los estados con mayor cantidad de casos reportados y los más claros indican una menor incidencia.</p>
                <p>Se observa que las entidades del centro del país, como el Estado de México y sus alrededores, registran un mayor número de casos, lo que se refleja en el color rojo intenso. Por otro lado, varios estados del norte y del sur presentan niveles significativamente más bajos, mostrados en tonalidades de rojo más claras.</p>
            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 34 -->
<!-- Imagen 35 -->
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/mexico_2022.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 35" data-toggle="modal" data-target="#modalImagen35">
        <div class="card-body">
            <h5 class="card-title">Distribución Geográfica de Secuestros en México en 2022</h5>
            <p class="card-text">Mapa que ilustra la distribución geográfica de secuestros en México durante 2022, con el Estado de México destacando en casos reportados.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 35 -->
<div class="modal fade" id="modalImagen35" tabindex="-1" aria-labelledby="modalLabel35" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel35">Distribución Geográfica de Secuestros en México en 2022</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/mexico_2022.png'); ?>" class="img-fluid" alt="Imagen 35">
                <p class="mt-3">Este mapa muestra la distribución de los secuestros registrados en México durante el año 2022. Los tonos de azul en el mapa indican la cantidad de secuestros por estado: los tonos más oscuros representan una mayor incidencia, mientras que los tonos más claros indican una menor cantidad de casos. Se observa que el Estado de México destaca significativamente con el mayor número de reportes, reflejado en el color azul más intenso. Por otro lado, estados como Baja California Sur y Campeche muestran una incidencia mucho menor, evidenciada en los tonos de azul más claros. Esta visualización permite identificar patrones regionales y áreas con alta incidencia de secuestros durante el periodo analizado.</p>
            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 35 -->
<!-- Imagen 36 --> 
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/mexico_2023.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 36" data-toggle="modal" data-target="#modalImagen36">
        <div class="card-body">
            <h5 class="card-title">Distribución Geográfica de Secuestros en México en 2023</h5>
            <p class="card-text">Mapa que representa la distribución geográfica de los secuestros en México durante el año 2023, destacando regiones con mayor incidencia.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 36 -->
<div class="modal fade" id="modalImagen36" tabindex="-1" aria-labelledby="modalLabel36" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel36">Distribución Geográfica de Secuestros en México en 2023</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/mexico_2023.png'); ?>" class="img-fluid" alt="Imagen 36">
                <p class="mt-3">Este mapa muestra la distribución de secuestros reportados en México en 2023, utilizando una escala de colores púrpura para representar la cantidad de incidentes en cada estado. Los tonos más oscuros indican un mayor número de casos, destacándose el estado de Tamaulipas como el de mayor incidencia, seguido por entidades como el Estado de México. Por otro lado, los tonos más claros reflejan estados con menor cantidad de reportes. Este análisis visual nos ayuda a identificar las zonas con mayor problemática de secuestros durante este año, permitiendo observar patrones regionales.</p>
            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 36 -->

<!-- Imagen 37 -->
<div class="col-md-4 mb-4">
    <div class="card h-100">
        <img src="<?= base_url('images/mexico_2024.png'); ?>" class="card-img-top img-thumbnail" alt="Imagen 37" data-toggle="modal" data-target="#modalImagen37">
        <div class="card-body">
            <h5 class="card-title">Distribución Geográfica de Secuestros en México de enero a octubre 2024</h5>
            <p class="card-text">Mapa que muestra la distribución de secuestros reportados en México durante enero a octubre de 2024, destacando las regiones con mayor incidencia.</p>
        </div>
    </div>
</div>

<!-- Modal Imagen 37 -->
<div class="modal fade" id="modalImagen37" tabindex="-1" aria-labelledby="modalLabel37" aria-hidden="true">
    <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
        <div class="modal-content">
            <div class="modal-header">
                <h5 class="modal-title" id="modalLabel37">Distribución Geográfica de Secuestros en México en 2024</h5>
                <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                    <span aria-hidden="true">&times;</span>
                </button>
            </div>
            <div class="modal-body">
                <img src="<?= base_url('images/mexico_2024.png'); ?>" class="img-fluid" alt="Imagen 37">
                <p class="mt-3">
                    Este mapa ilustra la distribución geográfica de los secuestros en México de enero a octubre 2024. Los estados están representados con una escala de tonos azules, donde los tonos más oscuros indican una mayor incidencia de secuestros. En este año, el Estado de México resalta como la entidad con el mayor número de casos reportados, seguido por otras regiones como Tamaulipas y Veracruz. Las tonalidades más claras reflejan estados con menos casos registrados, mientras que algunas áreas están marcadas en gris, indicando falta de datos reportados. Este análisis ayuda a identificar los patrones regionales de incidencia y las áreas más afectadas en el país.
                </p>
            </div>
        </div>
    </div>
</div>
<!-- Fin del Modal Imagen 37 -->



    <!-- Continúa agregando más imágenes aquí -->
</div>


</div>

<?= $this->include('templates/footer'); ?>
