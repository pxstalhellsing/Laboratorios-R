# ============================================================
# UNIVERSIDAD AUTONOMA DE CHILE
# Ingenieria Civil Informatica
# Probabilidad y Estadistica Computacional - Laboratorio 3
# PLANTILLA ESTUDIANTE
# Tecnicas de conteo: modelar, verificar y auditar
# Fecha: 19-08-2026
# ============================================================

options(digits = 12)


# 0. FUNCION DE APOYO ----------------------------------------

# P(n,r): selecciones ordenadas sin repeticion.
P <- function(n, r) {
  if (length(n) != 1 || length(r) != 1 ||
      n < 0 || r < 0 || r > n ||
      n != floor(n) || r != floor(r)) {
    stop("Se requieren enteros con 0 <= r <= n")
  }
  factorial(n) / factorial(n - r)
}


# 1. RECUPERACION: AUDITORIA DE FORMULAS --------------------

# Antes de ejecutar, predice cada resultado y explica:
# (a) si importa el orden; (b) si se permite repeticion.

auditoria <- c(
  codigo_distinto = P(8,5),  # P(8,5)
  equipo = choose(12,4),           # C(12,4)
  banana = factorial(6)/(factorial(3)*factorial(2)),           # 6!/(3!2!)
  podio = P(15,3)             # P(15,3)
)

auditoria


# 2. PRACTICA GUIADA A: BLOQUE, EXCLUSION Y SIMETRIA --------

# Procesos A,...,G: A y B juntos; C no va primero; D antes de E.
# Completa una expresion por linea. No escribas 600 directamente.

total_bloque <- factorial(6)*2
c_primero <- factorial(5)*2 
c_no_primero <- total_bloque - c_primero
validas <- c_no_primero/2

c(
  bloque = total_bloque,
  c_primero = c_primero,
  c_no_primero = c_no_primero,
  respuesta = validas
)


# 3. PRACTICA GUIADA B: CONTEO POR CASOS --------------------

# Equipo de 5: 6 software, 5 redes, 4 datos; al menos uno de
# cada area y cantidad impar de especialistas de datos.

casos3 <- expand.grid(
  s = 0:5,
  r = 0:5,
  d = 0:4
)

# Completa las tres condiciones dentro de subset().
casos3 <- subset(
  casos3,
  s + r + d == 5 &
  s >= 1 &
  r >= 1 &
  d >= 1 &
  d %% 2 == 1
)

# Completa el producto de combinaciones.
casos3$aporte <- with(
  casos3,
  # COMPLETA
  choose(6, s) * choose(5, r) * choose(4, d)
)

casos3
total_casos3 <- sum(casos3$aporte)
total_casos3


# 4. PRACTICA AUTONOMA A: COMITE CON RESTRICCIONES ----------

# Comite de 6: 8 software, 6 redes, 4 datos; al menos dos de
# datos y al menos una persona de software y una de redes.

casos4 <- expand.grid(
  s = 0:6,
  r = 0:6,
  d = 0:4
)

# Escribe el filtro completo.
casos4 <- subset(
  casos4,
  # COMPLETA
  s + r + d == 6 &
  s >= 1 &
  r >= 1 &
  d >= 2
)

# Escribe el aporte de cada composicion.
casos4$aporte <- with(
  casos4,
  # COMPLETA
  choose(8, s) *
  choose(6, r) *
  choose(4, d)
)

casos4
total_comites <- sum(casos4$aporte)
total_comites


# 5. PRACTICA AUTONOMA B: ASIGNACION SOBREYECTIVA -----------

# Ocho microservicios distintos se asignan a cuatro servidores
# distintos y todos los servidores deben recibir al menos uno.

# 5.1 Construye la expresion de inclusion-exclusion.
n_ie <- 4^8 -
  choose(4, 1) * 3^8 +
  choose(4, 2) * 2^8 -
  choose(4, 3) * 1^8

n_ie

# 5.2 Enumeracion exacta de las 4^8 asignaciones.
asignaciones <- expand.grid(rep(list(1:4), 8))

# Completa la condicion: una fila es valida si usa los 4 valores.
usan_todos <- apply(
  asignaciones,
  1,
  function(x) {
    # COMPLETA
    length(unique(x)) == 4
  }
)

n_enum <- sum(usan_todos)
c(inclusion_exclusion = n_ie, enumeracion = n_enum)

# Auditoria final: debe ser TRUE cuando ambos metodos coincidan.
identicos <- identical(as.numeric(n_ie), as.numeric(n_enum))
identicos


# 6. EXTENSION: TOKEN CON POSICIONES RESTRINGIDAS ------------

# Nueve posiciones: 4 letras, 3 digitos y 2 simbolos.
# La primera posicion es L y la ultima no puede ser S.

patrones <- expand.grid(
  rep(list(c("L", "D", "S")), 9),
  stringsAsFactors = FALSE
)
m <- as.matrix(patrones)

# Completa las cinco condiciones unidas con &.
patron_valido <- rowSums(m == "L") == 4 &
                rowSums(m == "D") == 3 &
                rowSums(m == "S") == 2 &
                m[, 1] == "L" &
                m[, 9] != "S"
  

n_patrones <- sum(patron_valido)
n_tokens <- n_tokens <- n_patrones * P(26, 4) * P(10, 3) * P(6, 2)  

c(patrones_validos = n_patrones, tokens = n_tokens)


# 7. CIERRE REPRODUCIBLE ------------------------------------

stopifnot(total_casos3 == 1360)
stopifnot(nrow(casos4) == 6)
stopifnot(all(casos4$aporte > 0))
stopifnot(total_comites == 6696)
stopifnot(total_comites < choose(18, 6))
stopifnot(n_ie == 40824)
stopifnot(n_enum == 40824)
stopifnot(identicos)
stopifnot(n_patrones == 420)
stopifnot(n_tokens == 3255033600000)

cat("Todos los controles fueron superados correctamente.\n")




