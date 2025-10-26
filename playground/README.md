# Hebi4 Playground

An interactive web-based playground for the Hebi4 programming language, built with Rust, WebAssembly, and CodeMirror 6.

## Features

- **Code Editor**: CodeMirror 6-based editor with syntax highlighting
- **Multiple Output Modes**:
  - **Run**: Execute code and see the result
  - **Disassembly**: View the compiled bytecode
  - **AST**: Inspect the abstract syntax tree
- **Optimization**: Toggle dead code elimination (DCE)
- **Examples**: Pre-loaded example programs
- **Code Sharing**: Share code via URL hash encoding
- **Responsive**: Works on desktop and mobile

## Development

### Prerequisites

- Rust toolchain (edition 2024)
- [wasm-pack](https://rustwasm.github.io/wasm-pack/) - Build Rust to WebAssembly
- Node.js and npm

### Setup

1. Install dependencies:
```bash
npm install
```

2. Run development server:
```bash
npm run dev
```

3. Build for production:
```bash
npm run build
```

The built files will be in the `dist/` directory.

### Available Scripts

- `npm run wasm` - Build WASM in dev mode
- `npm run wasm:release` - Build WASM in release mode
- `npm run dev` - Build WASM + start Vite dev server
- `npm run build` - Build WASM + bundle for production
- `npm run preview` - Preview production build

## Architecture

### Structure

```
playground/
├── src/
│   └── lib.rs           # Rust → WASM bindings
├── Cargo.toml           # Rust package config
├── index.html           # Main HTML page
├── style.css            # Playground styling
├── main.js              # Frontend logic
├── examples.js          # Example programs
├── package.json         # npm dependencies
└── dist/                # Built files (generated)
```

### WASM Bindings

The `lib.rs` file exposes four main functions to JavaScript:

- `parse(code)` - Parse code and return AST
- `disassemble(code, optimize)` - Compile and disassemble code
- `run(code, optimize)` - Execute code and return result
- `get_version()` - Get Hebi4 version

Each function returns a result object with `success` and `output` fields.

### Frontend

- **CodeMirror 6**: Bundled from npm, single consistent version
- **Vanilla JavaScript**: No framework dependencies, keeping it lightweight
- **Vite**: Bundles JavaScript and handles module resolution
- **wasm-pack**: Compiles Rust to WebAssembly with JS bindings
- **vite-plugin-wasm**: Enables seamless WASM imports in Vite

## Deployment

The `dist/` directory contains static files that can be served by any web server:

```bash
# Preview the production build
npm run preview

# Or manually with Python
python -m http.server --directory dist 8000

# Or with Node.js http-server
npx http-server dist
```

For production deployment, consider using:
- GitHub Pages
- Netlify
- Vercel
- Any static site hosting service

## Future Enhancements

- [ ] Tree-sitter integration for advanced syntax highlighting
- [ ] Interactive debugging
- [ ] Performance profiling
- [ ] Multi-file support
- [ ] Export/import code
- [ ] Dark/light theme toggle

## License

Licensed under the same terms as the main Hebi4 project (MIT/Apache-2.0).
