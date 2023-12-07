# proyectoFuncional
Proyecto para FUncional y concurrente Universidad del valle 2023-2
jdk JAVA 17.0.6
sbt 1.9.7
scala 2.13.8 \n
El proyecto esta desarrollado como se planteo en el enunciado, pero contiene algunas carpetas con paquetes que no estaban previstas en el.
    -La carpeta benchmark contiene un paquete con el fin de solucionar y hacer funciones para tomar tiempos, tanto comparando funciones como de forma individual.
En Pruebas.sc están implementadas algunas funciones que se crearon con el fin de hacer las pruebas del benchmark de manera automatizada, de igual manera, todas estas funciones estan comentadas para su entendimiento y el archivo Pruebas.sc contiene todas las pruebas que hicimos para sacar los resultados que mostramos en el informe.
El build.sbt está modificado para hacer uso del scalameter como se planteo en el enunciado y para la utilizacion de el paralelismo de datos. Esta modificación es la misma que se planteo en el taller 5.
En la carpeta RESULTADOS, estan los resultados obtenidos en unidades de segundos.
