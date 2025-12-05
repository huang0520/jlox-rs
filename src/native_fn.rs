use std::time::{SystemTime, UNIX_EPOCH};

use crate::literal::Literal;

pub fn clock(_: &[Literal]) -> Literal {
    Literal::Number(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("time should go forward")
            .as_secs() as f64,
    )
}
