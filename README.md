# IRSL

Índice de Rezago Social Longitudinal (IRSL), 2000-2020
## Setup Instructions

The following steps outline how to prepare your environment to run the IRSL calculation scripts.

1. Ensure you have **R 4.0 or newer** and **RStudio** installed.
2. Clone or download this repository.
3. Open `proyecto_IRSL.Rproj` in RStudio.
4. Install the required packages:

   ```r
   install.packages(c("data.table", "foreign", "tidyverse", "stratification", "fpc"))
   ```

5. Copy the census and survey data into the `bases` folder if they are not already included.
6. Run `ITER_15.R` to generate variables for the 2015 Intercensal Survey.
7. Execute `programa_de_calculo_IRSL.R` or `programa_de_calculo_RSL_entidades.R` as needed.


Los programas de cálculo del Índice de Rezago Social Longitudinal (IRSL) utilizan las bases del Conteo de Población y Vivienda 2005 y el Censo de Población y Vivienda 2000, 2010 y 2020, las cuales se encuentran en la carpeta "bases" de este proyecto. En el caso del año 2015 se utilizan las bases de la "Encuesta Intercensal 2015" para lo cual se deben crear las variables necesarias para el cálculo del IRSL, en este sentido se pone a su disposición el programa de cálculo "ITER_15.R" en el cual se muestra el proceso de construcción de las variables antes mencionadas para su posterior uso en los programas de cálculo del IRSL. Es importante mencionar que para el funcionamiento del programa de cálculo "ITER_15.R" es necesario contar con las bases de datos de la "Encuesta Intercensal 2015" en formato .csv, las cuales están disponibles en el sitio del INEGI https://www.inegi.org.mx/programas/intercensal/2015/#Microdatos

Para un mejor funcionamiento de este programa de cálculo se recomienda seguir los siguientes pasos:

1. Descomprimir el contenido del archivo .zip en el directorio de su preferencia
2. Abrir el proyecto "proyecto_IRSL.Rproj"
3. Es necesario tener instalado R versión 4 o superior 
4. Una vez que se abra el proyecto se pueden ejecutar los programas de cálculo para el IRSL

Por un lado, el archivo "programa_de_calculo_IRSL.R" contiene el programa de cálculo para estimar el IRSL para las desagregaciones territoriales: estatal, municipal y localidad de 2000 a 2020. Mientras que, el archivo "programa_de_calculo_RSL_entidades" contiene el programa de cálculo para las estimaciones del IRSL de las localidades al interior de las entidades de 2000 a 2020.

Es importante considerar que el código está escrito en el paquete estadístico R, a través de su IDE RStudio. Para su ejecución se usó la versión de R 4.2.2 y RStudio versión 2022.12.0+353, además las versiones de los paquetes son las siguientes:

- data.table_1.14.6
- foreign_0.8-84
- tidyverse_1.3.2
- stratification_2.2-7
- fpc_2.2-10
cfuentes@coneval.org.mx

Tel: 54817200
Ext: 70100, 70106, y 70070
