// use rocket::serde::{Deserialize, Serialize};
use serde::{Deserialize, Serialize};
use sqlx::FromRow;

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct PageEvent {
    pub page: String,
    pub uuid: String,
    #[serde(rename = "details")]
    pub event: Event,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Event {
    Daily(DailyEvent),
    TrainingFinished(TrainingFinishedEvent),
    TrainingRound(TrainingRoundEvent),
    NumbersRound(NumbersRoundEvent),
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct DailyEvent {
    #[serde(rename = "dicts-active")]
    dicts_active: Vec<String>,
    progress: String,
    result: String,
    attempts: i32,
    mistakes: i32,
    rata: i32,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct TrainingFinishedEvent {
    #[serde(rename = "dicts-active")]
    dicts_active: Vec<String>,
    attempts: i32,
    correct: i32,
    retries: i32,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct TrainingRoundEvent {
    #[serde(rename = "dicts-active")]
    dicts_active: Vec<String>,
    #[serde(rename = "nr-words-found")]
    nr_words_found: i32,
    success: bool,
    mistakes: i32,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct NumbersRoundEvent {
    mode: String,
    number: i64,
}

#[derive(Serialize, FromRow)]
pub struct PageStats {
    pub daily_count: i64,
    pub training_count: i64,
    pub numbers_count: i64,
}

#[cfg(test)]
mod tests {
    use super::*;
    // use serde::{Deserialize, Serialize};

    #[test]
    fn daily_event() {
        let ev = PageEvent {
            page: String::from("daily"),
            uuid: String::from("mock-uuid"),
            event: Event::Daily(DailyEvent {
                dicts_active: vec![String::from("some-dict1"), String::from("some-dict2")],
                progress: String::from("progress"),
                result: String::from("result"),
                attempts: 100,
                mistakes: 10,
                rata: 2000,
            }),
        };

        let as_string = serde_json::to_string(&ev).unwrap();
        let and_back = serde_json::from_str(&as_string).unwrap();

        assert_eq!(ev, and_back);
    }
}
