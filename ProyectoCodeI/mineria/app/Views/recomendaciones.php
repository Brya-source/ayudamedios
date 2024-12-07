<?= $this->include('templates/header'); ?>

<div class="container mt-5">
    <h1 class="text-center mb-4">Recomendaciones</h1>

    <!-- Subencabezado "Recomendaciones" -->
    <h2 class="mt-5 mb-4">Recomendaciones mediante minería de datos.</h2>
    <p>
        Partimos de datos filtrados para identificar patrones en las incidencias reportadas. Consideramos variables categóricas 
        como estado, municipio, captor, lugar, tipo de secuestro y método de captura. Aplicamos Análisis de Correspondencias Múltiples (MCA)
        para transformar estas variables en componentes numéricos. Utilizamos clusterización jerárquica con dichos componentes 
        para formar grupos con características observables. Con estos grupos generamos recomendaciones dirigidas tanto a las autoridades 
        como a la población, atendiendo las condiciones identificadas en cada agrupación. Posteriormente, calculamos distancias a los centroides 
        para detectar casos atípicos.
    </p>

    <div class="row">
        <!-- Imagen 1 de Recomendaciones -->
        <div class="col-md-4 mb-4">
            <div class="card h-100">
                <img src="<?= base_url('images/screeplot.png'); ?>" class="card-img-top img-thumbnail" alt="Recomendación 1" data-toggle="modal" data-target="#modalRecomendacion1">
                <div class="card-body">
                    <h5 class="card-title">Gráfico Scree de la Varianza Explicada por las Dimensiones</h5>
                    <p class="card-text">Este gráfico muestra la proporción de varianza explicada por cada dimensión resultante del Análisis de Correspondencias Múltiples. Nos ayuda a decidir cuántas dimensiones conservar para nuestro análisis.</p>
                </div>
            </div>
        </div>

        <!-- Modal Recomendación 1 -->
        <div class="modal fade" id="modalRecomendacion1" tabindex="-1" aria-labelledby="modalRecomendacionLabel1" aria-hidden="true">
            <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
                <div class="modal-content">
                    <div class="modal-header">
                        <h5 class="modal-title" id="modalRecomendacionLabel1">Gráfico Scree Detallado</h5>
                        <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                            <span aria-hidden="true">&times;</span>
                        </button>
                    </div>
                    <div class="modal-body">
                        <img src="<?= base_url('images/screeplot.png'); ?>" class="img-fluid" alt="Recomendación 1">
                        <p class="mt-3">
                            En este gráfico, cada barra representa la cantidad de varianza que explica una dimensión 
                            al transformar nuestras variables categóricas en componentes numéricos. La línea que conecta 
                            las marcas refleja la disminución progresiva de la varianza explicada a medida que avanzamos hacia 
                            dimensiones de menor relevancia. Observamos que las primeras dimensiones explican una mayor proporción 
                            de la varianza, mientras que las posteriores agregan menos información.

                            Tomamos las primeras cuatro dimensiones porque, en conjunto, proporcionan un punto de equilibrio 
                            entre la complejidad del modelo y la capacidad de describir adecuadamente las relaciones entre las 
                            variables. Al retener estas cuatro dimensiones, mantenemos información suficiente para identificar 
                            patrones y tendencias, evitando al mismo tiempo una representación demasiado extensa o con poca 
                            aportación adicional.
                        </p>
                    </div>
                </div>
            </div>
        </div>
        <!-- Fin del Modal Recomendación 1 -->

        <!-- Imagen 2 de Recomendaciones -->
        <div class="col-md-4 mb-4">
            <div class="card h-100">
                <img src="<?= base_url('images/indice_silueta7.png'); ?>" class="card-img-top img-thumbnail" alt="Recomendación 2" data-toggle="modal" data-target="#modalRecomendacion2">
                <div class="card-body">
                    <h5 class="card-title">Validación del Número de Clústeres (Índice de Silueta)</h5>
                    <p class="card-text">Este gráfico muestra cómo varía el índice de silueta según la cantidad de clústeres formados, ayudándonos a determinar cuántos clústeres utilizar tras haber seleccionado un cierto número de dimensiones.</p>
                </div>
            </div>
        </div>

        <!-- Modal Recomendación 2 -->
        <div class="modal fade" id="modalRecomendacion2" tabindex="-1" aria-labelledby="modalRecomendacionLabel2" aria-hidden="true">
            <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
                <div class="modal-content">
                    <div class="modal-header">
                        <h5 class="modal-title" id="modalRecomendacionLabel2">Validación del Número de Clústeres (Índice de Silueta) Detallada</h5>
                        <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                            <span aria-hidden="true">&times;</span>
                        </button>
                    </div>
                    <div class="modal-body">
                        <img src="<?= base_url('images/indice_silueta7.png'); ?>" class="img-fluid" alt="Recomendación 2">
                        <p class="mt-3">
                            En este gráfico, el eje horizontal representa la cantidad de clústeres que consideramos, mientras 
                            que el eje vertical indica el índice de silueta promedio asociado a cada partición. El índice de silueta 
                            evalúa qué tan bien separados están los clústeres entre sí y cuán coherentes son internamente.

                            A medida que incrementamos el número de clústeres, observamos cambios en el índice de silueta. Nosotros 
                            buscamos el punto en el cual este índice es mayor, pues allí se logra un equilibrio entre la separación 
                            y la coherencia de los grupos. Este análisis, combinado con las dimensiones seleccionadas anteriormente, 
                            nos guía en la elección del número óptimo de clústeres.
                        </p>
                    </div>
                </div>
            </div>
        </div>
        <!-- Fin del Modal Recomendación 2 -->

        <!-- Imagen 3 de Recomendaciones -->
        <div class="col-md-4 mb-4">
            <div class="card h-100">
                <img src="<?= base_url('images/dendrograma4.png'); ?>" class="card-img-top img-thumbnail" alt="Recomendación 3" data-toggle="modal" data-target="#modalRecomendacion3">
                <div class="card-body">
                    <h5 class="card-title">Dendrograma de Clusterización Jerárquica con MCA</h5>
                    <p class="card-text">Este gráfico muestra cómo se forman y separan los grupos a partir de datos categóricos reducidos mediante Análisis de Correspondencias Múltiples (MCA), organizándolos en distintos niveles de similitud.</p>
                </div>
            </div>
        </div>

        <!-- Modal Recomendación 3 -->
        <div class="modal fade" id="modalRecomendacion3" tabindex="-1" aria-labelledby="modalRecomendacionLabel3" aria-hidden="true">
            <div class="modal-dialog modal-lg modal-dialog-centered modal-dialog-scrollable">
                <div class="modal-content">
                    <div class="modal-header">
                        <h5 class="modal-title" id="modalRecomendacionLabel3">Dendrograma de Clusterización Jerárquica con MCA Detallada</h5>
                        <button type="button" class="close" data-dismiss="modal" aria-label="Cerrar">
                            <span aria-hidden="true">&times;</span>
                        </button>
                    </div>
                    <div class="modal-body">
                        <img src="<?= base_url('images/dendrograma4.png'); ?>" class="img-fluid" alt="Recomendación 3">
                        <p class="mt-3">
                            En este dendrograma, cada unión de ramas representa la formación de clústeres que agrupan casos con características semejantes. 
                            Para obtenerlo, primero transformamos nuestras variables categóricas con MCA, reduciendo la complejidad del conjunto de datos. 
                            Luego aplicamos clusterización jerárquica, un proceso que va uniendo, paso a paso, las observaciones más similares. La altura en el eje vertical 
                            indica la distancia a la que se fusionan los clústeres, permitiéndonos identificar el número de grupos a retener. Con esta representación visual, 
                            podemos decidir en qué nivel cortar el dendrograma, basándonos en análisis previos (como el índice de silueta), y así obtener un número adecuado de 
                            clústeres para orientar recomendaciones y acciones.
                        </p>
                    </div>
                </div>
            </div>
        </div>
        <!-- Fin del Modal Recomendación 3 -->
    </div>

    <!-- Diccionario de términos -->
    <h2 class="mt-5 mb-4">Diccionario de Términos</h2>
    <p>Para facilitar la comprensión de los términos utilizados entre comillas en las recomendaciones, aquí se presentan sus definiciones:</p>
    <dl>
        <dt>confianza</dt>
        <dd>Situación en la que la víctima es capturada por personas de su entorno cercano (amigos, familiares o conocidos), aprovechando una relación previa de familiaridad y confianza.</dd>

        <dt>vía publica</dt>
        <dd>La captura ocurre en espacios públicos, como calles o lugares abiertos, donde la víctima puede ser interceptada fuera de su domicilio.</dd>

        <dt>emboscada</dt>
        <dd>La víctima es sorprendida mediante una acción planificada, rodeada o interceptada repentinamente, sin oportunidad de escapar.</dd>

        <dt>casa</dt>
        <dd>La captura se realiza en el hogar de la víctima, ya sea dentro o en las inmediaciones de su domicilio.</dd>

        <dt>cartel</dt>
        <dd>La acción es perpetrada por un grupo criminal organizado (cártel u otra organización delictiva) que emplea sus recursos para llevar a cabo el secuestro.</dd>

        <dt>autoridad</dt>
        <dd>La víctima es capturada por personas que fingen ser o son realmente integrantes de cuerpos de seguridad (policías, militares) abusando de su autoridad.</dd>

        <dt>fuerza</dt>
        <dd>Se emplea violencia física para someter a la víctima (golpear, empujar, sujetar con fuerza o utilizar armas).</dd>

        <dt>complicidad</dt>
        <dd>El secuestro cuenta con la participación o ayuda de alguien del entorno de la víctima (un cómplice interno).</dd>

        <dt>persona comun</dt>
        <dd>La captura es realizada por individuos sin relación de confianza, sin ser autoridad ni pertenecer a un grupo criminal especializado.</dd>

        <dt>transporte</dt>
        <dd>La víctima es capturada en relación con un medio de transporte (autobús, taxi, transporte público), al abordarlo o al descender.</dd>

        <dt>intimidación</dt>
        <dd>La víctima es amenazada o coaccionada verbalmente, sin empleo directo de la fuerza física, para someter su voluntad.</dd>

        <dt>suplantación de identidad</dt>
        <dd>Los captores se hacen pasar por otras personas (autoridades, conocidos, funcionarios) para engañar a la víctima y capturarla bajo una falsa apariencia.</dd>

        <dt>tecnológica</dt>
        <dd>La captura se organiza a través de medios digitales (redes sociales, apps, internet) donde se contacta y engaña a la víctima previamente.</dd>

        <dt>secuestro general</dt>
        <dd>Acto de privar ilegalmente de la libertad a una persona contra su voluntad, sin especificar el método o motivación.</dd>

        <dt>secuestro extorsivo</dt>
        <dd>Secuestro realizado con el propósito de obtener una ganancia económica u otro beneficio a cambio de la liberación de la víctima.</dd>
    </dl>


<!--
<h2 class="mt-5 mb-4">Recomendaciones (Fallback sin BD)</h2>
<p>Este bloque es por si la base de datos no está disponible para mostrar las recomendaciones directamente desde el código.</p>
<div class="table-responsive">
    <table class="table table-bordered table-striped">
        <thead class="thead-dark">
            <tr>
                <th>Tipo de Recomendación</th>
                <th>Cluster</th>
                <th>Estado</th>
                <th>Municipio</th>
                <th>Recomendación</th>
            </tr>
        </thead>
        <tbody>
            <tr>
                <td>Estado</td><td>1</td><td>Nuevo León</td><td>Monterrey</td><td>En el municipio de Monterrey del estado Nuevo León, se observa una alta incidencia de secuestros del tipo 'Secuestro extorsivo', frecuentemente perpetrados por 'confianza'. Se recomienda al estado implementar medidas de seguridad enfocadas en 'vía pública'.</td>
            </tr>
            <tr>
                <td>Poblacion</td><td>1</td><td>Nuevo León</td><td>Monterrey</td><td>Se recomienda a la población del municipio de Monterrey en el estado Nuevo León, tener precaución ante posibles secuestros realizados mediante 'fuerza' en 'vía pública'. Manténgase alerta y tome medidas preventivas.</td>
            </tr>
            <tr>
                <td>Estado</td><td>2</td><td>Estado de México</td><td>Toluca</td><td>En el municipio de Toluca del estado Estado de México, se observa una alta incidencia de secuestros del tipo 'Secuestro extorsivo', frecuentemente perpetrados por 'confianza'. Se recomienda al estado implementar medidas de seguridad enfocadas en 'vía pública'.</td>
            </tr>
            <tr>
                <td>Poblacion</td><td>2</td><td>Estado de México</td><td>Toluca</td><td>Se recomienda a la población del municipio de Toluca en el estado Estado de México, tener precaución ante posibles secuestros realizados mediante 'emboscada' en 'vía pública'. Manténgase alerta y tome medidas preventivas.</td>
            </tr>
            <tr>
                <td>Estado</td><td>3</td><td>Ciudad de México</td><td>Iztapalapa</td><td>En el municipio de Iztapalapa del estado Ciudad de México, se observa una alta incidencia de secuestros del tipo 'Secuestro extorsivo', frecuentemente perpetrados por 'confianza'. Se recomienda al estado implementar medidas de seguridad enfocadas en 'casa'.</td>
            </tr>
            <tr>
                <td>Poblacion</td><td>3</td><td>Ciudad de México</td><td>Iztapalapa</td><td>Se recomienda a la población del municipio de Iztapalapa en el estado Ciudad de México, tener precaución ante posibles secuestros realizados mediante 'emboscada' en 'casa'. Manténgase alerta y tome medidas preventivas.</td>
            </tr>
            <tr>
                <td>Estado</td><td>4</td><td>Tamaulipas</td><td>Reynosa</td><td>En el municipio de Reynosa del estado Tamaulipas, se observa una alta incidencia de secuestros del tipo 'Secuestro extorsivo', frecuentemente perpetrados por 'cartel'. Se recomienda al estado implementar medidas de seguridad enfocadas en 'casa'.</td>
            </tr>
            <tr>
                <td>Poblacion</td><td>4</td><td>Tamaulipas</td><td>Reynosa</td><td>Se recomienda a la población del municipio de Reynosa en el estado Tamaulipas, tener precaución ante posibles secuestros realizados mediante 'emboscada' en 'casa'. Manténgase alerta y tome medidas preventivas.</td>
            </tr>
            <tr>
                <td>Estado</td><td>5</td><td>Michoacán</td><td>Madero</td><td>En el municipio de Madero del estado Michoacán, se observa una alta incidencia de secuestros del tipo 'Secuestro extorsivo', frecuentemente perpetrados por 'autoridad'. Se recomienda al estado implementar medidas de seguridad enfocadas en 'casa'.</td>
            </tr>
            <tr>
                <td>Poblacion</td><td>5</td><td>Michoacán</td><td>Madero</td><td>Se recomienda a la población del municipio de Madero en el estado Michoacán, tener precaución ante posibles secuestros realizados mediante 'fuerza' en 'casa'. Manténgase alerta y tome medidas preventivas.</td>
            </tr>
            <tr>
                <td>Estado</td><td>6</td><td>Morelos</td><td>Cuernavaca</td><td>En el municipio de Cuernavaca del estado Morelos, se observa una alta incidencia de secuestros del tipo 'Secuestro general', frecuentemente perpetrados por 'complicidad'. Se recomienda al estado implementar medidas de seguridad enfocadas en 'casa'.</td>
            </tr>
            <tr>
                <td>Poblacion</td><td>6</td><td>Morelos</td><td>Cuernavaca</td><td>Se recomienda a la población del municipio de Cuernavaca en el estado Morelos, tener precaución ante posibles secuestros realizados mediante 'emboscada' en 'casa'. Manténgase alerta y tome medidas preventivas.</td>
            </tr>
            <tr>
                <td>Estado</td><td>7</td><td>Coahuila</td><td>Zaragoza</td><td>En el municipio de Zaragoza del estado Coahuila, se observa una alta incidencia de secuestros del tipo 'Secuestro general', frecuentemente perpetrados por 'persona común'. Se recomienda al estado implementar medidas de seguridad enfocadas en 'vía pública'.</td>
            </tr>
            <tr>
                <td>Poblacion</td><td>7</td><td>Coahuila</td><td>Zaragoza</td><td>Se recomienda a la población del municipio de Zaragoza en el estado Coahuila, tener precaución ante posibles secuestros realizados mediante 'emboscada' en 'vía pública'. Manténgase alerta y tome medidas preventivas.</td>
            </tr>
            <tr>
                <td>Caso Aislado</td><td></td><td>Jalisco</td><td>Puerto Vallarta</td><td>En el municipio de Puerto Vallarta del estado de Jalisco, se ha identificado un caso particular de secuestro de tipo 'Secuestro extorsivo', realizado mediante 'emboscada', perpetrado por 'confianza' en el lugar 'casa'. Se recomienda investigar este caso en detalle y considerar medidas específicas.</td>
            </tr>
            <tr>
                <td>Caso Aislado</td><td></td><td>Jalisco</td><td>Puerto Vallarta</td><td>En el municipio de Puerto Vallarta del estado de Jalisco, se ha identificado un caso particular de secuestro de tipo 'Secuestro extorsivo', realizado mediante 'fuerza', perpetrado por 'autoridad' en el lugar 'transporte'. Se recomienda investigar este caso en detalle y considerar medidas específicas.</td>
            </tr>
            <tr>
                <td>Caso Aislado</td><td></td><td>Estado de México</td><td>Ecatepec de Morelos</td><td>En el municipio de Ecatepec de Morelos del estado de Estado de México, se ha identificado un caso particular de secuestro de tipo 'Secuestro extorsivo', realizado mediante 'fuerza', perpetrado por 'complicidad' en el lugar 'casa'. Se recomienda investigar este caso en detalle y considerar medidas específicas.</td>
            </tr>
            <tr>
                <td>Caso Aislado</td><td></td><td>Jalisco</td><td>Puerto Vallarta</td><td>En el municipio de Puerto Vallarta del estado de Jalisco, se ha identificado un caso particular de secuestro de tipo 'Secuestro extorsivo', realizado mediante 'fuerza', perpetrado por 'cartel' en el lugar 'vía pública'. Se recomienda investigar este caso en detalle y considerar medidas específicas.</td>
            </tr>
            <tr>
                <td>Caso Aislado</td><td></td><td>Guerrero</td><td>Chilpancingo de los Bravo</td><td>En el municipio de Chilpancingo de los Bravo del estado de Guerrero, se ha identificado un caso particular de secuestro de tipo 'Secuestro general', realizado mediante 'emboscada', perpetrado por 'cartel' en el lugar 'transporte'. Se recomienda investigar este caso en detalle y considerar medidas específicas.</td>
            </tr>
            <tr>
                <td>Caso Aislado</td><td></td><td>Michoacán</td><td>Zamora</td><td>En el municipio de Zamora del estado de Michoacán, se ha identificado un caso particular de secuestro de tipo 'Secuestro extorsivo', realizado mediante 'intimidacion', perpetrado por 'suplantación de identidad' en el lugar 'casa'. Se recomienda investigar este caso en detalle y considerar medidas específicas.</td>
            </tr>
            <tr>
                <td>Caso Aislado</td><td></td><td>Guerrero</td><td>Chilpancingo de los Bravo</td><td>En el municipio de Chilpancingo de los Bravo del estado de Guerrero, se ha identificado un caso particular de secuestro de tipo 'Secuestro general', realizado mediante 'fuerza', perpetrado por 'confianza' en el lugar 'transporte'. Se recomienda investigar este caso en detalle y considerar medidas específicas.</td>
            </tr>
            <tr>
                <td>Caso Aislado</td><td></td><td>Estado de México</td><td>Toluca</td><td>En el municipio de Toluca del estado de Estado de México, se ha identificado un caso particular de secuestro de tipo 'Secuestro general', realizado mediante 'tecnologica', perpetrado por 'complicidad' en el lugar 'transporte'. Se recomienda investigar este caso en detalle y considerar medidas específicas.</td>
            </tr>
            <tr>
                <td>Caso Aislado</td><td></td><td>Guerrero</td><td>Chilpancingo de los Bravo</td><td>En el municipio de Chilpancingo de los Bravo del estado de Guerrero, se ha identificado un caso particular de secuestro de tipo 'Secuestro general', realizado mediante 'fuerza', perpetrado por 'complicidad' en el lugar 'transporte'. Se recomienda investigar este caso en detalle y considerar medidas específicas.</td>
            </tr>
            <tr>
                <td>Caso Aislado</td><td></td><td>Nuevo León</td><td>Monterrey</td><td>En el municipio de Monterrey del estado de Nuevo León, se ha identificado un caso particular de secuestro de tipo 'Secuestro extorsivo', realizado mediante 'fuerza', perpetrado por 'suplantación de identidad' en el lugar 'transporte'. Se recomienda investigar este caso en detalle y considerar medidas específicas.</td>
            </tr>
            <tr>
                <td>Caso Aislado</td><td></td><td>Guerrero</td><td>Chilpancingo de los Bravo</td><td>En el municipio de Chilpancingo de los Bravo del estado de Guerrero, se ha identificado un caso particular de secuestro de tipo 'Secuestro general', realizado mediante 'intimidacion', perpetrado por 'confianza' en el lugar 'transporte'. Se recomienda investigar este caso en detalle y considerar medidas específicas.</td>
            </tr>
        </tbody>
    </table>
</div>
 -->


    <!-- Nuevo contenedor para mostrar datos de la DB -->
    <h2 class="mt-5 mb-4">Recomendaciones extraídas de la Base de Datos</h2>
    <p>
        A continuación se presentan recomendaciones obtenidas directamente desde la base de datos. 
        Estas han sido generadas a partir de la interpretación del clúster jerárquico y asignadas a 
        una plantilla predefinida.
    </p>
    <div class="table-responsive">
        <table class="table table-bordered table-striped">
            <thead class="thead-dark">
                <tr>
                    <th>ID</th>
                    <th>Tipo de Recomendación</th>
                    <th>Cluster</th>
                    <th>Estado</th>
                    <th>Municipio</th>
                    <th>Recomendación</th>
                </tr>
            </thead>
            <tbody>
                <?php if (!empty($recomendaciones_db)): ?>
                    <?php foreach ($recomendaciones_db as $row): ?>
                        <tr>
                            <td><?= esc($row['id']); ?></td>
                            <td><?= esc($row['tipo_recomendacion']); ?></td>
                            <td><?= esc($row['cluster']); ?></td>
                            <td><?= esc($row['estado']); ?></td>
                            <td><?= esc($row['municipio']); ?></td>
                            <td><?= esc($row['recomendacion']); ?></td>
                        </tr>
                    <?php endforeach; ?>
                <?php else: ?>
                    <tr>
                        <td colspan="6" class="text-center">No se encontraron recomendaciones en la base de datos.</td>
                    </tr>
                <?php endif; ?>
            </tbody>
        </table>
    </div>
    <!-- Fin del contenedor de datos DB -->

</div>

<?= $this->include('templates/footer'); ?>
