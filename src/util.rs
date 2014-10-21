
pub macro_rules! try_as(
  ($e:expr, $wrap:ident) => (try!($e.map_err(|inner| $wrap(inner))))
)