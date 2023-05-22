use criterion::{black_box, criterion_group, criterion_main, Criterion};
use sql_parser::parse;

fn basic_queries(c: &mut Criterion) {
    let mut group = c.benchmark_group("base_query");

    let query_text = "select * from a";
    group.bench_function("select", |b| {
        b.iter(|| parse(black_box(query_text)).unwrap());
    });

    let query_text = "select a.b as a, c.d as c from w.q as w";
    group.bench_function("select colum 1", |b| {
        b.iter(|| parse(black_box(query_text)).unwrap());
    });

    let query_text = "select * from a inner join b using (q, w, e)";
    group.bench_function("select inner join", |b| {
        b.iter(|| parse(black_box(query_text)).unwrap());
    });

    let query_text = "select * from a where a in (1,2,3) and b = 1 * (1 + 2)";
    group.bench_function("select where 1", |b| {
        b.iter(|| parse(black_box(query_text)).unwrap());
    });
}

criterion_group!(benches, basic_queries);
criterion_main!(benches);
