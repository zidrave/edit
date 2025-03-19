fn main() {
    if std::env::var("CARGO_CFG_TARGET_OS").unwrap_or_default() == "windows"
        && std::env::var("CARGO_CFG_TARGET_ENV").unwrap_or_default() == "msvc"
    {
        let path = std::path::absolute("src/edit.exe.manifest").unwrap();
        let path = path.to_str().unwrap();
        println!("cargo::rerun-if-changed=src/edit.exe.manifest");
        println!("cargo::rustc-link-arg-bin=edit=/MANIFEST:EMBED");
        println!("cargo::rustc-link-arg-bin=edit=/MANIFESTINPUT:{}", path);
        println!("cargo::rustc-link-arg-bin=edit=/WX");
    }
}
