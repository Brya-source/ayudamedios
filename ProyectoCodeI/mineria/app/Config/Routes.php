<?php

use CodeIgniter\Router\RouteCollection;

/**
 * @var RouteCollection $routes
 */


$routes->get('/', 'Home::index');
$routes->get('/galeria', 'Galeria::index');
$routes->get('recomendaciones', 'Recomendaciones::index');
