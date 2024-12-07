<?php

namespace App\Controllers;

use CodeIgniter\Controller;

class Galeria extends Controller
{
    public function index() 
    {
        // Puedes pasar datos a la vista si es necesario
        return view('galeria');
    }
}
