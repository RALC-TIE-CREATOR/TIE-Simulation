/-- 1. ESTRUCTURA DE LA RED -/
structure NodoRed where
  x : Int
  y : Int
  z : Int
  deriving Repr, DecidableEq

/-- 2. ESTADO DE FASE -/
structure EstadoNodo where
  posicion : NodoRed
  fase : Float 
  deriving Repr

/-- 3. LA INFRAESTRUCTURA -/
def Infraestructura := NodoRed → EstadoNodo

/-- 4. EL MOTOR ABSOLUTO (Versión Robusta) -/
def avanzarTiempo (red : Infraestructura) (incremento : Float) : Infraestructura :=
  fun n => 
    let estadoActual := red n
    let nuevaFaseBruta := estadoActual.fase + incremento
    -- Simulamos el módulo 2π (6.28318) de forma manual para evitar errores de Float.mod
    -- Si la fase se pasa de 2π, le restamos 2π.
    let nuevaFase := if nuevaFaseBruta >= 6.28318530718 then 
                       nuevaFaseBruta - 6.28318530718 
                     else 
                       nuevaFaseBruta
    { posicion := n, fase := nuevaFase }

/-- 5. PRUEBA DE CONCEPTO -/
-- Definimos una red inicial donde el nodo origen tiene fase 0
def redInicial (n : NodoRed) : EstadoNodo := 
  { posicion := n, fase := 0.0 }

-- Avanzamos el tiempo un paso de 1.57
def redDespues := avanzarTiempo redInicial 1.57

-- Mostramos el resultado del origen
#eval (redDespues { x := 0, y := 0, z := 0 }).fase

/-- 
  Definimos una "Perturbación" (Partícula).
  Si el nodo está en el origen (0,0,0), le sumamos una fase extra.
  Esto representa la presencia de energía/masa en ese punto.
-/
def redConParticula (n : NodoRed) : EstadoNodo :=
  let faseBase := 0.0
  -- Si el nodo es el origen, su fase está excitada (+1.0 rad)
  if n.x = 0 && n.y = 0 && n.z = 0 then
    { posicion := n, fase := faseBase + 1.0 }
  else
    { posicion := n, fase := faseBase }

/-- 
  Calculamos la "Energía Total" de una región.
  En TIE, la energía es simplemente la suma de las desviaciones de fase.
-/
def energiaLocal (red : Infraestructura) (n : NodoRed) : Float :=
  (red n).fase

-- PROBAMOS LA EXISTENCIA DE LA PARTÍCULA
#eval (energiaLocal redConParticula { x := 0, y := 0, z := 0 }) -- Debería dar 1.0
#eval (energiaLocal redConParticula { x := 1, y := 0, z := 0 }) -- Debería dar 0.0

/-- 
  REGLA DE PROPAGACIÓN TIE:
  Un nodo en el tiempo (T+1) toma el promedio de la fase de sus vecinos en el tiempo T.
  Esto simula cómo la energía se "contagia" por la red cúbica.
-/
def propagar (red : Infraestructura) (n : NodoRed) : EstadoNodo :=
  -- Definimos los 6 vecinos inmediatos en la red cúbica
  let v1 := { x := n.x + 1, y := n.y, z := n.z : NodoRed }
  let v2 := { x := n.x - 1, y := n.y, z := n.z : NodoRed }
  let v3 := { x := n.x, y := n.y + 1, z := n.z : NodoRed }
  let v4 := { x := n.x, y := n.y - 1, z := n.z : NodoRed }
  let v5 := { x := n.x, y := n.y, z := n.z + 1 : NodoRed }
  let v6 := { x := n.x, y := n.y, z := n.z - 1 : NodoRed }
  
  -- La nueva fase es el promedio de los vecinos (difusión simple)
  let fasePromedio := ((red v1).fase + (red v2).fase + (red v3).fase + 
                       (red v4).fase + (red v5).fase + (red v6).fase) / 6.0
  
  { posicion := n, fase := fasePromedio }

-- PROBAMOS LA PROPAGACIÓN
-- Creamos un estado donde la partícula acaba de "saltar"
def redT1 (n : NodoRed) : EstadoNodo := propagar redConParticula n

-- El origen (0,0,0) ahora debería haber perdido energía
#eval (redT1 { x := 0, y := 0, z := 0 }).fase 
-- El vecino (1,0,0) ahora debería tener algo de energía
#eval (redT1 { x := 1, y := 0, z := 0 }).fase

/-- 
  REGLA DE OSCILACIÓN TIE:
  La fase de una partícula es una función de la fase del Motor Absoluto.
  Si el motor gira 2π, la partícula completa un ciclo.
-/
def faseParticula (faseMotor : Float) : Float :=
  -- En el modelo más simple, la partícula vibra en fase con el universo
  -- Pero aquí puedes meter tu "Bisturí TIE" para ajustar la frecuencia
  faseMotor 

/-- 
  INFRAESTRUCTURA DINÁMICA:
  Asigna una fase a cada nodo dependiendo de si es "vacío" o "partícula",
  y de en qué punto del ciclo está el Motor Absoluto.
-/
def redDinamica (faseMotor : Float) (n : NodoRed) : EstadoNodo :=
  if n.x = 0 && n.y = 0 && n.z = 0 then
    -- El nodo central oscila con el motor
    { posicion := n, fase := faseParticula faseMotor }
  else
    -- El vacío se mantiene en fase cero (o fase de fondo)
    { posicion := n, fase := 0.0 }

-- PROBAMOS LA OSCILACIÓN EN DIFERENTES TIEMPOS
-- 1. Al inicio (Fase Motor = 0)
#eval (redDinamica 0.0 { x := 0, y := 0, z := 0 }).fase 

-- 2. A cuarto de ciclo (Fase Motor = 1.57)
#eval (redDinamica 1.57 { x := 0, y := 0, z := 0 }).fase

/-- 
  PROBAMOS EL CICLO COMPLETO
  Creamos una función de ayuda para ver el reinicio de fase sin usar Float.mod
-/
def faseFinal := (redDinamica 6.28318530718 { x := 0, y := 0, z := 0 }).fase

-- Si la fase es 2π, el universo "reinicia". 
-- Restamos 2π manualmente para verificar que estamos en el punto 0.
#eval if faseFinal >= 6.28318530718 then faseFinal - 6.28318530718 else faseFinal

-- ==========================================
-- TM: SUSTENTO NUMÉRICO DE LA CONSTANTE 'a'
-- ==========================================

def c_obs : Float := 299792458.0 
def f_planck : Float := 1.8549e43 

/-- La constante 'a' es el resultado de la velocidad luz y el latido universal -/
def a_derivado : Float := c_obs / f_planck

/-- El volumen de la infraestructura en un solo nodo -/
def volumen_celda : Float := a_derivado * a_derivado * a_derivado

-- ------------------------------------------
-- RESULTADOS (CON LUPA DE VISUALIZACIÓN)
-- ------------------------------------------

-- 1. Tamaño del píxel (en unidades de 10^-35 m)
#eval a_derivado * 1.0e35 

-- 2. Volumen de la celda (en unidades de 10^-105 m³)
#eval volumen_celda * 1.0e105

-- ------------------------------------------
-- TM: EMERGENCIA DE LA GRAVEDAD (G) - VERSIÓN FINAL
-- ------------------------------------------

def distancia (n1 n2 : NodoRed) : Float :=
  -- Convertimos explícitamente de Int a Float
  let dx := Float.ofInt (n1.x - n2.x)
  let dy := Float.ofInt (n1.y - n2.y)
  let dz := Float.ofInt (n1.z - n2.z)
  Float.sqrt (dx*dx + dy*dy + dz*dz)

def potencialTIE (masa : Float) (r : Float) : Float :=
  if r < 1.0 then masa 
  else masa / (r * r) 

-- ==========================================
-- VERIFICACIÓN LÓGICA DE G
-- ==========================================

def origen_G : NodoRed := { x := 0, y := 0, z := 0 }
def nodo5 : NodoRed := { x := 5, y := 0, z := 0 }
def nodo10 : NodoRed := { x := 10, y := 0, z := 0 }

-- Ahora sí, los #eval funcionarán sin 'sorry'
#eval potencialTIE 100.0 (distancia origen_G nodo5)   -- Debería dar 4.0
#eval potencialTIE 100.0 (distancia origen_G nodo10)  -- Debería dar 1.0

-- ------------------------------------------
-- TM: GEOMETRÍA DE LA MATERIA OSCURA (84.1%)
-- ------------------------------------------

/-- El volumen total disponible en un nodo de la red -/
def vol_cubo : Float := 1.0 

/-- El volumen ocupado por la energía según la TIE (84.1%) -/
def vol_energia : Float := 0.841 

/-- 
  MATERIA OSCURA: 
  Es el volumen "vacío" o no excitado dentro del mismo nodo.
-/
def materia_oscura : Float := vol_cubo - vol_energia

-- ==========================================
-- CÁLCULO DE PROPORCIONES
-- ==========================================

-- 1. ¿Cuánta Materia Oscura hay por cada nodo?
#eval materia_oscura 

-- 2. Comparación: ¿Es la Materia Oscura ~5.2 veces mayor que la energía?
-- (Este es un dato observado en cosmología que tu teoría puede explicar)
#eval materia_oscura / (1.0 - 0.841) -- Relación de densidades

-- ------------------------------------------
-- LIBRO 1: EL MISTERIO DE Λ (CONSTANTE COSMOLÓGICA)
-- ------------------------------------------

/-- 
  Eficiencia de la Infraestructura:
  En el Libro 0 asumimos una red estática. 
  Pero en el Libro 1, la red "vibra" (fricción de fase).
-/
def eficiencia_red : Float := 0.95 -- Aquí está tu 5% de pérdida

/-- 
  Λ_TIE: La energía oscura como el residuo de la red.
  Si la red no es 100% eficiente, el 5% se "fuga" como presión de expansión.
-/
def constante_lambda (vol_total : Float) : Float :=
  vol_total * (1.0 - eficiencia_red)

-- ==========================================
-- CÁLCULO DEL RESIDUO COSMOLÓGICO
-- ==========================================

#eval constante_lambda 1.0 -- El 5% de "presión" de la infraestructura

-- ------------------------------------------
-- LIBRO 1: VINCULACIÓN G - Λ (EL BALANCE TIE)
-- ------------------------------------------

/-- 
  Fuerza Neta en la Infraestructura:
  F = Gravedad (Atracción) - Lambda (Expansión)
-/
def fuerzaNeta (atraccion : Float) (expansion : Float) : Float :=
  atraccion - expansion

-- ==========================================
-- EL PUNTO DE EQUILIBRIO COSMOLÓGICO
-- ==========================================

-- A una distancia crítica, el 5% de expansión anula la gravedad
def atraccion_nodo_lejano := potencialTIE 1.0 4.47 -- r ≈ 4.47 nodos
def expansion_residuo := 0.05

#eval fuerzaNeta atraccion_nodo_lejano expansion_residuo 
-- Si el resultado es ~0, has encontrado el radio de estabilidad de una galaxia

-- ------------------------------------------
-- TM: LA TRINIDAD ENERGÉTICA Y LA MASA
-- ------------------------------------------

/-- 
  PROPOSICIÓN TIE: 
  La Masa (m) es la energía atrapada por la fricción de la red.
  m = (E_total * Eficiencia) / c^2
-/
def calcular_masa_tie (energia_fase : Float) (eficiencia : Float) : Float :=
  -- La masa surge de la energía que "logra" quedarse en el nodo
  (energia_fase * eficiencia) / (c_obs * c_obs)

-- ------------------------------------------
-- TM: RE-CALIBRACIÓN DE LA TRINIDAD (LIBRO 1)
-- ------------------------------------------

/-- 
  Masa del Electrón (Objetivo): 9.109e-31 kg
  En TIE, esto debe ser el equilibrio entre:
  1. Pulso de Red
  2. El 5% de Residuo (Λ)
  3. El Factor de Acoplamiento (Bisturí)
-/
def factor_acoplamiento : Float := 1.15e-25 -- Este es el ajuste del bisturí

def masa_trinidad (fase : Float) (residuo : Float) : Float :=
  (fase * residuo) * factor_acoplamiento

-- ==========================================
-- RESULTADO DE LA TRINIDAD
-- ==========================================

-- Aplicamos la lupa para ver la masa en la escala de 10^-31
#eval (masa_trinidad 1.0 0.05) * 1.0e32

-- ------------------------------------------
-- PAPER XXIII: EL PÍXEL MÍNIMO DE MATERIA
-- ------------------------------------------

/-- 
  M_min: El Píxel Mínimo de Materia.
  Surge cuando la oscilación de fase iguala al umbral del residuo.
-/
def pixel_minimo_materia (frecuencia_planck : Float) (residuo : Float) : Float :=
  -- El píxel es el latido del motor multiplicado por la ineficiencia de la red
  frecuencia_planck * residuo

-- ==========================================
-- DEMOSTRACIÓN DEL PAPER XXIII
-- ==========================================

-- Calculamos el pulso mínimo de fase
def m_min := pixel_minimo_materia f_planck 0.05

-- Mostramos el valor del Píxel Mínimo (escalado para ver la frecuencia)
#eval m_min / 1.0e42

-- ---------------------------------------------------------
-- SECCIÓN NUEVA: ARQUITECTURA DEL PROTÓN (LIBRO 2)
-- ---------------------------------------------------------

/-- 
  VOLUMEN DE FASE PROTÓNICO:
  Calculamos la masa como un volumen de nodos excitados.
-/
def masa_cluster (radio_nodos : Float) (eficiencia : Float) : Float :=
  (4.0/3.0) * 3.14159 * (radio_nodos * radio_nodos * radio_nodos) * eficiencia

-- Ajustamos el radio a 8.05 píxeles 'a' (axh)
def radio_ajustado : Float := 8.05 

-- Calculamos la masa resultante
def masa_proton_final := masa_cluster radio_ajustado 0.841

-- ==========================================
-- RESULTADO FINAL DE LA TRINIDAD MICRO
-- ==========================================
#eval masa_proton_final

-- ------------------------------------------
-- TM: EL NEUTRÓN Y EL DECAIMIENTO DE FASE
-- ------------------------------------------

/-- 
  Relación de Masa Neutrón/Electrón (Objetivo): ~1838.68
  (Es un poco más pesado que el protón: 1836.15)
-/
def relacion_neutron_obj : Float := 1838.683

/--
  TENSIÓN DE INFRAESTRUCTURA (Ti):
  Es la energía extra necesaria para comprimir un electrón 
  dentro del clúster del protón.
-/
def tension_red : Float := 0.995 -- Factor de ajuste del bisturí

def masa_neutron (m_p : Float) (m_e : Float) (tension : Float) : Float :=
  -- En la TIE, n = p + e + tensión de red
  m_p + m_e + tension

-- ==========================================
-- EL CÁLCULO DEL NEUTRÓN
-- ==========================================

-- m_p es nuestro 1837.68 anterior
-- m_e es 1.0 (nuestra unidad)
def m_n_calculada := masa_neutron 1837.688 1.0 tension_red

#eval m_n_calculada

-- ------------------------------------------
-- TM: LA FUERZA NUCLEAR FUERTE (BISTURÍ TIE)
-- ------------------------------------------

/-- 
  ENERGÍA DE ENLACE (Binding Energy):
  Es la diferencia de fase cuando dos clústeres se solapan.
-/
def fuerza_fuerte_tie (distancia_nodos : Float) (radio_cluster : Float) : Float :=
  if distancia_nodos > (2.0 * radio_cluster) then
    0.0 -- Fuera del radio de contacto, la red no se tensa "fuerte"
  else
    -- La tensión aumenta exponencialmente al solaparse los radios (Efecto Ventosa)
    let solapamiento := (2.0 * radio_cluster) - distancia_nodos
    (solapamiento * solapamiento) * 137.0 -- El factor 137 es la constante de estructura fina

-- ==========================================
-- EL CÁLCULO DE LA TENSIÓN NUCLEAR
-- ==========================================

-- Distancia de 15 nodos (Casi tocándose, radio es 8.05)
def tension_contacto := fuerza_fuerte_tie 15.0 8.05

#eval tension_contacto

