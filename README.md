# 💻🗂️ Codigos autolisp
Codigos de ejercicios realizados en la clase de computación grafica y de trabajos realizados para la automatización de tareas. 

## ⌚✍🏼 Reloj analogo digital

Este reloj consiste de presentar al usuario la hora de forma analoga y digital con la fecha actual junto al día de la semana correspondiente a la fecha. Para esto se realiza el siguiente proceso:

1. Se solicita la cantidad de segundos que se desea mantener en ejecución el reloj.
2. Se da un punto en la ventana de dibujo el cual sera el centro del reloj.
3. Un valor de escala en que se quiere este el dibujo del reloj (velor base 100mm).

Una vez se comience la ejecución del programa se ara una limpieza de la ventana de dibujo, se solicitaran los datos anteriormente mencionados al usuario. Luego se hara el trazado del dibujo se abstraera del sistema los datos necesarios de hora y fecha, se actualizara el reloj con estos datos, se hara zoom al reloj para su correcta visualización y se ejecuta la animación.

Para ejecutar el programa del reloj solo tenemos que haber cargado el archivo “RelojAnalogoDigital.lsp” en Autocad luego llamamos la función Reloj la cual esta como una funcion de autocad para lo cual solo debemos escribir “Reloj” en la consola de Autocad.

## 🖥️✍🏼📖 Sala CAD 

La sala cad es una sala la cual se encuentra en el edificio de postgrado en materiales de la Universidad Nacional de Colombia Sede Bogotá. En este programa se hizo:

1. Un dibujo base a mano apartir de bloques de dibujo.
2. Converitr todo el dibujo en un archivo .txt en formato CDF o SDF con los datos correspondientes de cada bloque. 
3. Se realizo un preprocesado en excel de los archivos .txt para separalos por su formato correspondiente.
4. En un nuvo archivo de dibujo se puede hacer la reconstrucción del dibujo original.

Para ejecutar el programa de la sala cad se debe cargar el archivo "ReconstruccionSalaCAD.lsp", se ejecuta la función ExtraerAtributos con esta funcion podemos crear los archivos .txt y luego se puede ejecutar la función RecontruirDibujo la cual abre el buscador de archivos para seleccionar el archivo de excel a recuperar los datos y recontruye todo el dibujo.
