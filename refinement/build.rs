fn main() {
    if let Err(e) = lalrpop::process_root() {
        eprintln!("Failed to process lalrpop grammar: {}", e);
        std::process::exit(1);
    }
}
