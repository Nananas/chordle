#[macro_use]
extern crate rocket;

pub mod api;

use crate::api::*;

use chrono::Utc;
use rocket::fs::{FileServer, Options};
use rocket::http::Status;
use rocket::serde::json::Json;
use rocket_db_pools::sqlx;
use rocket_db_pools::{Connection, Database};
use serde_json::to_string;
use sqlx::FromRow;
use std::path::Path;

mod catchers {
    use rocket::catch;
    use rocket::Request;
    // Custom error handlers
    #[catch(404)]
    pub fn not_found(_: &Request) -> &'static str {
        "不好意思，没找到。404"
    }

    #[catch(400)]
    pub fn bad_request(_: &Request) -> &'static str {
        "不好意思，我做错了。500"
    }

    #[catch(500)]
    pub fn internal_server_error(_: &Request) -> &'static str {
        "不好意思，您做错了。500"
    }
}

fn ok_response() -> (Status, String) {
    (Status::Ok, "OK".to_owned())
}

#[derive(Database)]
#[database("chordle")]
struct Db(sqlx::PgPool);

#[post("/event", format = "application/json", data = "<page_event>")]
async fn route_event(mut db: Connection<Db>, page_event: Json<PageEvent>) -> (Status, String) {
    match to_string(&page_event.event) {
        Ok(ev) => {
            let now = Utc::now();

            let q = sqlx::query(
                "insert into page_events(page, uuid, details, timestamp) values ($1, $2, $3, $4)",
            )
            .bind(&page_event.page)
            .bind(&page_event.uuid)
            .bind(&ev)
            .bind(now.to_rfc3339())
            .execute(&mut *db)
            .await;
            match q {
                Ok(_) => ok_response(),
                Err(err) => (Status::InternalServerError, err.to_string()),
            }
        }
        Err(err) => (Status::BadRequest, err.to_string()),
    }
}

#[get("/stats")]
async fn route_stats(mut db: Connection<Db>) -> (Status, String) {
    let mut daily_count = 0;
    let mut training_count = 0;
    let mut numbers_count = 0;

    #[derive(FromRow)]
    struct PageStatsRow {
        page: String,
        count: i64,
    }

    let stat_result =
        sqlx::query_as::<_, PageStatsRow>("select page, count(*) from page_events group by page")
            .fetch_all(&mut *db)
            .await;

    match stat_result {
        Ok(rows) => {
            for row in rows {
                match row.page.as_str() {
                    "daily" => daily_count = row.count,
                    "training" => training_count = row.count,
                    "numbers" => numbers_count = row.count,
                    _ => {}
                }
            }

            match to_string(&PageStats {
                daily_count: daily_count,
                training_count: training_count,
                numbers_count: numbers_count,
            }) {
                Ok(s) => (Status::Ok, s),
                Err(e) => (Status::InternalServerError, e.to_string()),
            }
        }
        Err(err) => (Status::InternalServerError, err.to_string()),
    }
}

// This launch macro will generate a main which will setup an async tokio runtime
#[launch]
fn rocket() -> _ {
    rocket::build()
        .attach(Db::init())
        .mount(
            "/",
            FileServer::new(Path::new("./static"), Options::None | Options::Index),
        )
        .mount("/", routes![route_event, route_stats])
        .register(
            "/",
            catchers![
                catchers::not_found,
                catchers::bad_request,
                catchers::internal_server_error
            ],
        )
}
