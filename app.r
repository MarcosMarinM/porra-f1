# Cargar librerías
library(shiny)
library(shinymanager)

# --- 1. Definir la base de datos de usuarios ---
# Por ahora, usamos un data.frame. ¡Más adelante hashearemos las claves!
credentials <- data.frame(
  user = c("admin", "amigo1", "amigo2"),
  password = c("password", "clave1", "clave2"),
  # Permisos (admin o standard)
  admin = c(TRUE, FALSE, FALSE),
  stringsAsFactors = FALSE
)


# --- 2. Definir la Interfaz de Usuario (UI) ---
# Esta es la UI *después* de que el usuario inicie sesión.
ui <- fluidPage(
  titlePanel("La carrera más surrealist"),
  
  # Sidebar para mostrar quién está conectado
  sidebarLayout(
    sidebarPanel(
      h4("Usuario conectado:"),
      # 'shinymanager' crea un output llamado 'auth_output'
      # que podemos usar para mostrar el nombre de usuario
      verbatimTextOutput("auth_output")
    ),
    
    # Panel principal
    mainPanel(
      h2("¡Bienvenidx a la porra!")
      # Aquí irá el formulario
    )
  )
)


# --- 3. Definir la lógica del Servidor (Server) ---
server <- function(input, output, session) {
  
  # --- Control de autenticación ---
  # 'res_auth' es un objeto reactivo que contiene la info del login
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  # --- Lógica de la app (solo visible si estás logueado) ---
  
  # Mostrar el nombre de usuario
  output$auth_output <- renderPrint({
    # 'res_auth' tiene la info del usuario
    paste("Usuario:", res_auth$user)
  })
  
  # ... Aquí irá la lógica para guardar la porra
}


# --- 4. Envolver la UI con el login ---
# 'secure_app' añade la pantalla de login a nuestra 'ui'
# Opcional: podemos traducir los campos de login
ui_con_login <- secure_app(
  ui,
  language = "es"
)


# --- 5. Ejecutar la Aplicación ---
# Usamos 'ui_con_login' en lugar de 'ui'
shinyApp(ui = ui_con_login, server = server)