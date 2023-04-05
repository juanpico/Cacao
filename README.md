# Cacao
En este repositorio se encuentran los scripts y algunos archivos de datos para el proyecto de grado orientado a la predicción de productividad de cacao en el departamento del Meta.

La estructura es la siguiente:

0. **Datos IDEAM**: En esta carpeta se realiza en análisis exploratorio de los datos meteorológicos.
1. **Generación de instancias**: Scripts y datos relacionados a la generación de datos meteorológicos aleatorios y a la ejecución del modelo de simulación fisiológica del cacao.
2.  **Modelo predictivo**: Contiene el código para entrenar el modelo de ML utilizado y los datos de entrenamiento.
3.  **Módulo estimación**: Scripts y datos necesarios para realizar predicciones sobre la productividad del cacao con base en el modelo de ML y una simulación de Monte Carlo.
4.  **Validación fincas**: Contiene el código para realizar la predicción de la productividad en siete cultivos del Meta.
5.  **Herramienta**: En esta carpeta se encuentra la herramienta en Shiny para realizar predicciones sobre nuevos cultivos.

El enlace para acceder a la herramienta en Shiny es el siguiente: https://juancamilopico.shinyapps.io/Herramienta/

A continuación se observa una pre-visualización de la predicción realizada por la herramienta.

<img width="568" alt="image" src="https://user-images.githubusercontent.com/75444742/230235191-4aca63c7-f26b-4cb5-9d36-42764f3f3c8c.png">

