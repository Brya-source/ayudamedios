<?php

namespace App\Controllers;

use CodeIgniter\Controller;
use Config\Database;

class Recomendaciones extends Controller
{
    public function index()
    {

        // Conexión a la base de datos utilizando la configuración del .env
        $db = Database::connect();

        // Ejecutar la consulta (ajusta el nombre de la tabla y campos según tu caso)
        $query = $db->query("SELECT * FROM recomendaciones");
        $resultados = $query->getResultArray();

        // Pasamos los resultados a la vista
        $data['recomendaciones_db'] = $resultados;

        return view('recomendaciones', $data);
    }
}
