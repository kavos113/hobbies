use axum::{
    extract::rejection::JsonRejection, http::StatusCode, routing::{get, post}, Json, Router
};
use axum::extract::Query;
use sqlx::mysql::MySqlConnectOptions;

#[derive(sqlx::FromRow)]
#[sqlx(rename_all = "PascalCase")]
#[allow(dead_code)]
struct City {
    #[sqlx(rename = "ID")]
    id: i32,
    name: String,
    country_code: String,
    district: String,
    population: i32,
}

fn get_option() -> anyhow::Result<MySqlConnectOptions> {
    // let host = env::var("DB_HOSTNAME")?;
    // let port = env::var("DB_PORT")?.parse()?;
    // let username = env::var("DB_USER")?;
    // let password = env::var("DB_PASSWORD")?;
    // let database = env::var("DB_NAME")?;
    let host = String::from("localhost");
    let port = 3306;
    let username = String::from("root");
    let password = String::from("password");
    let database = String::from("world");
    let timezone = Some(String::from("Asia/Tokyo"));
    let collation = String::from("utf8mb4_general_ci");

    println!("host: {}, port: {}, username: {}, password: {}, database: {}", host, port, username, password, database);

    Ok(MySqlConnectOptions::new()
        .host(&host)
        .port(port)
        .username(&username)
        .password(&password)
        .database(&database)
        .timezone(timezone)
        .collation(&collation))
}

#[tokio::main]
async fn main() {
    let options = get_option().unwrap();
    let pool = sqlx::MySqlPool::connect_with(options).await.unwrap();
    println!("Connected to MySQL");

    let city = sqlx::query_as::<_, City>("SELECT * FROM city WHERE Name = ?")
        .bind("Tokyo")
        .fetch_one(&pool)
        .await
        .map_err(|e| match e {
            sqlx::Error::RowNotFound => println!("City not found"),
            _ => println!("Error: {:?}", e),
        })
        .unwrap();
    println!("city's population: {}", city.population);

    let app = Router::new()
        .route("/", get(handler))
        .route("/json", get(json_handler))
        .route("/post", post(post_handler))
        .route("/query", get(query_handler));

    let listener = tokio::net::TcpListener::bind("127.0.0.1:8080")
        .await
        .unwrap();

    axum::serve(listener, app)
        .await
        .unwrap();
}

async fn handler() -> String {
    String::from("Hello, World!")
}

#[derive(serde::Serialize, serde::Deserialize)]
struct JsonData {
    number: i32,
    string: String,
    bool: bool,
}

async fn json_handler() -> Json<JsonData> {
    let res = JsonData {
        number: 10,
        string: String::from("value"),
        bool: true,
    };

    Json(res)
}

async fn post_handler(
    query: Result<Json<JsonData>, JsonRejection>,
) -> Result<Json<JsonData>, (StatusCode, JsonRejection)> {
    match query {
        Ok(data) => Ok(data),
        Err(rejection) => Err((StatusCode::BAD_REQUEST, rejection)),
    }
}

#[derive(serde::Deserialize)]
#[serde(rename_all = "camelCase")]
struct QueryParam {
    id: i32,
    some_query: String,
}

async fn query_handler(
    Query(query): Query<QueryParam>
) -> String {
    format!("id: {}, some_query: {}", query.id, query.some_query)
}