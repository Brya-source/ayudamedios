<!-- app/Views/home.php -->
<?= $this->include('templates/header'); ?>

<div class="container mt-5">
    <div class="jumbotron text-center">
        <h1 class="display-4">Análisis de Secuestros en México</h1>
        <p class="lead">Exploración de los datos y tendencias relacionadas con los secuestros.</p>
        <hr class="my-4">
        <p>Descubre estadísticas y recomendaciones en nuestra galería interactiva.</p>
        <a class="btn btn-primary btn-lg" href="<?= base_url('galeria'); ?>" role="button">Ver Galería</a>
        <!-- Nuevo botón para recomendaciones -->
        <a class="btn btn-secondary btn-lg" href="<?= base_url('recomendaciones'); ?>" role="button">Ver Recomendaciones</a>
    </div>
    <div class="text-center">
        <img src="<?= base_url('images/mexicob_2024.png'); ?>" class="img-fluid" alt="Mexico2024">
    </div>
</div>

<?= $this->include('templates/footer'); ?>
