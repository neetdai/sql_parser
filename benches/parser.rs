use criterion::{criterion_group, criterion_main, Criterion};
use sql_parser::parse;

fn basic_queries(c: &mut Criterion) {
    let mut group = c.benchmark_group("base_query");

    let query_text = "select * from table";
    group.bench_function("select", |b| {
        b.iter(|| parse(query_text).unwrap());
    });

    let query_text = "select a.b as a, c.d as c from w.q as w";
    group.bench_function("select colum 1", |b| {
        b.iter(|| parse(query_text).unwrap());
    });
}

criterion_group!(benches, basic_queries);
criterion_main!(benches);