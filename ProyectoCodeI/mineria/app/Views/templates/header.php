<!-- app/Views/templates/header.php -->
<!DOCTYPE html>
<html lang="es">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Análisis de Secuestros</title>
    <!-- Bootstrap CSS -->
    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css">
    <!-- Font Awesome -->
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.1/css/all.min.css">
    <!-- Estilos Personalizados -->
    <link rel="stylesheet" href="<?= base_url('css/styles.css'); ?>">
    <!-- Meta etiquetas para SEO -->
    <meta name="description" content="Análisis de secuestros en México.">
    <meta name="keywords" content="secuestros, análisis, estadística, seguridad, prevención">
</head>
<body>
    <!-- Navbar -->
    <nav class="navbar navbar-expand-lg navbar-dark bg-dark">
        <!-- Escudo del IPN a la izquierda -->
        <a class="navbar-brand" href="https://www.ipn.mx/">
            <img src="<?= base_url('images/logo_ipnb.png'); ?>" alt="Escudo IPN" height="40">
        </a>

        <!-- Botón para dispositivos móviles -->
        <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarNav" aria-controls="navbarNav" aria-expanded="false" aria-label="Toggle navigation">
            <span class="navbar-toggler-icon"></span>
        </button>

        <!-- Contenido colapsable -->
        <div class="collapse navbar-collapse" id="navbarNav">
            <!-- Título o Nombre del Sitio -->
            <a class="navbar-brand mx-auto" href="<?= base_url('/'); ?>">Análisis de Secuestros</a>

            <!-- Enlaces de Navegación -->
            <ul class="navbar-nav mx-auto">
                <li class="nav-item">
                    <a class="nav-link" href="<?= base_url('/'); ?>">Inicio</a>
                </li>
                <li class="nav-item">
                    <a class="nav-link" href="<?= base_url('galeria'); ?>">Galería</a>
                </li>
                </li>
                <li class="nav-item">
                    <a class="nav-link" href="<?= base_url('recomendaciones'); ?>">Recomendaciones</a>
                </li>
                <!-- Más enlaces si lo deseas -->
            </ul>

            <!-- Escudo de UPIITA a la derecha -->
            <a class="navbar-brand" href="https://www.upiita.ipn.mx/">
                <img src="<?= base_url('images/logo_upiitab.png'); ?>" alt="Escudo UPIITA" height="40">
            </a>
        </div>
    </nav>





