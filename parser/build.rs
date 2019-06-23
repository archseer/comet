fn main() {
    lalrpop::process_root().unwrap();
    println!("grammar.lalrpop recompiled")
}
