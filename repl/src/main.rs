use structopt::clap::AppSettings;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(raw(
    global_settings = "&[AppSettings::ColoredHelp, AppSettings::VersionlessSubcommands]"
))]
enum Command {
    #[structopt(name = "build", about = "Compile Comet code")]
    Build {
        #[structopt(help = "location of the project root", default_value = ".")]
        path: String,
    },
}

fn main() {
    match Command::from_args() {
        Command::Build { path } => command_build(path),
    }
}

// TODO: use a root instead
fn command_build(file: String) {
    use comet_codegen as codegen;
    use comet_ir as ir;
    use comet_parser as parser;

    // load path
    let input = std::fs::read(file)
        .map(|s| String::from_utf8(s).unwrap())
        .unwrap();

    // parse
    let ast_module = parser::parser::parse(&input).expect("syntax error");
    println!("--> Parsed: {:#?}", ast_module);

    // lower
    let ir_module = ir::lower_module(&ast_module, "repl").expect("failed to lower");
    println!("--> Lowered: {:#?}", ir_module);

    // // compile
    let fun = codegen::compile(&ir_module).unwrap();
    println!("--> Compiled to LLVM: {:#?}", ir_module);
    fun.print_to_stderr();

    // println!("{}", root);
}
