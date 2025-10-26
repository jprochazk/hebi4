import { EditorView, basicSetup } from "codemirror";
import { EditorState } from "@codemirror/state";
import { keymap } from "@codemirror/view";
import { examples } from "./examples.js";
import init, { parse, disassemble, run } from "./pkg/hebi4_playground.js";

let editor;
let wasmReady = false;

// Initialize WASM
async function initWasm() {
  try {
    await init();
    wasmReady = true;
    console.log("Hebi4 WASM initialized");
    updateAllOutputs();
  } catch (err) {
    console.error("Failed to initialize WASM:", err);
    showError("run", "Failed to initialize WebAssembly module");
  }
}

// Create CodeMirror editor
function createEditor() {
  const defaultCode = getCodeFromUrl() || examples.welcome.code;

  const runCommand = () => {
    updateAllOutputs();
    return true;
  };

  const state = EditorState.create({
    doc: defaultCode,
    extensions: [
      basicSetup,
      EditorView.theme({
        "&": { height: "100%", backgroundColor: "#1e1e1e" },
        ".cm-content": { caretColor: "#528bff" },
        ".cm-cursor, .cm-dropCursor": { borderLeftColor: "#528bff" },
        "&.cm-focused .cm-selectionBackground, .cm-selectionBackground, .cm-content ::selection": {
          backgroundColor: "#264f78"
        },
        ".cm-gutters": {
          backgroundColor: "#1e1e1e",
          color: "#858585",
          border: "none"
        }
      }, { dark: true }),
      keymap.of([
        { key: "Ctrl-Enter", run: runCommand },
        { key: "Cmd-Enter", run: runCommand }
      ]),
      EditorView.updateListener.of((update) => {
        if (update.docChanged) {
          // Debounce updates
          clearTimeout(window.updateTimer);
          window.updateTimer = setTimeout(() => {
            saveCodeToStorage();
          }, 1000);
        }
      })
    ]
  });

  editor = new EditorView({
    state,
    parent: document.getElementById("editor")
  });
}

// Get current code from editor
function getCode() {
  return editor.state.doc.toString();
}

// Get optimization setting
function getOptimize() {
  return document.getElementById("optimize").checked;
}

// Update all outputs
function updateAllOutputs() {
  if (!wasmReady) {
    return;
  }

  const code = getCode();
  const optimize = getOptimize();

  // Update AST
  updateAst(code);

  // Update Disassembly
  updateDisasm(code, optimize);

  // Update Run output
  updateRun(code, optimize);
}

// Update AST output
function updateAst(code) {
  const output = document.getElementById("output-ast");

  if (!code.trim()) {
    output.textContent = "(empty program)";
    output.className = "output";
    if (getCurrentTab() === "ast") {
      output.classList.add("active");
    }
    return;
  }

  try {
    const result = parse(code);

    if (result.success) {
      output.textContent = result.output;
      output.className = "output";
    } else {
      output.textContent = result.output;
      output.className = "output error";
    }
  } catch (err) {
    output.textContent = `Error: ${err.message}`;
    output.className = "output error";
  }

  if (getCurrentTab() === "ast") {
    output.classList.add("active");
  }
}

// Update Disassembly output
function updateDisasm(code, optimize) {
  const output = document.getElementById("output-disasm");

  if (!code.trim()) {
    output.textContent = "(empty program)";
    output.className = "output";
    if (getCurrentTab() === "disasm") {
      output.classList.add("active");
    }
    return;
  }

  try {
    const result = disassemble(code, optimize);

    if (result.success) {
      output.textContent = result.output;
      output.className = "output";
    } else {
      output.textContent = result.output;
      output.className = "output error";
    }
  } catch (err) {
    output.textContent = `Error: ${err.message}`;
    output.className = "output error";
  }

  if (getCurrentTab() === "disasm") {
    output.classList.add("active");
  }
}

// Update Run output
function updateRun(code, optimize) {
  const output = document.getElementById("output-run");

  if (!code.trim()) {
    output.textContent = "(no code to run)";
    output.className = "output";
    if (getCurrentTab() === "run") {
      output.classList.add("active");
    }
    return;
  }

  try {
    const result = run(code, optimize);

    if (result.success) {
      output.textContent = result.output;
      output.className = "output success";
    } else {
      output.textContent = result.output;
      output.className = "output error";
    }
  } catch (err) {
    output.textContent = `Error: ${err.message}`;
    output.className = "output error";
  }

  if (getCurrentTab() === "run") {
    output.classList.add("active");
  }
}

// Show error in output
function showError(tab, message) {
  const output = document.getElementById(`output-${tab}`);
  output.textContent = message;
  output.className = "output error";
  if (getCurrentTab() === tab) {
    output.classList.add("active");
  }
}

// Get current active tab
function getCurrentTab() {
  const activeTab = document.querySelector(".tab.active");
  return activeTab ? activeTab.dataset.tab : "run";
}

// Setup tabs
function setupTabs() {
  const tabs = document.querySelectorAll(".tab");
  const outputs = document.querySelectorAll(".output");

  tabs.forEach(tab => {
    tab.addEventListener("click", () => {
      // Remove active class from all tabs and outputs
      tabs.forEach(t => t.classList.remove("active"));
      outputs.forEach(o => o.classList.remove("active"));

      // Add active class to clicked tab and corresponding output
      tab.classList.add("active");
      const tabName = tab.dataset.tab;
      document.getElementById(`output-${tabName}`).classList.add("active");
    });
  });
}

// Setup examples dropdown
function setupExamples() {
  const select = document.getElementById("examples");

  // Populate dropdown
  Object.entries(examples).forEach(([key, example]) => {
    const option = document.createElement("option");
    option.value = key;
    option.textContent = example.name;
    select.appendChild(option);
  });

  // Handle selection
  select.addEventListener("change", (e) => {
    const exampleKey = e.target.value;
    if (exampleKey && examples[exampleKey]) {
      setCode(examples[exampleKey].code);
      updateAllOutputs();
    }
    // Reset dropdown
    e.target.value = "";
  });
}

// Set code in editor
function setCode(code) {
  editor.dispatch({
    changes: {
      from: 0,
      to: editor.state.doc.length,
      insert: code
    }
  });
}

// Setup Run button
function setupRunButton() {
  document.getElementById("run").addEventListener("click", () => {
    updateAllOutputs();
  });
}

// Setup Optimize checkbox
function setupOptimizeCheckbox() {
  document.getElementById("optimize").addEventListener("change", () => {
    updateAllOutputs();
  });
}

// Setup Share button
function setupShareButton() {
  document.getElementById("share").addEventListener("click", async () => {
    const code = getCode();
    const url = new URL(window.location.href);
    url.hash = `code=${btoa(encodeURIComponent(code))}`;

    try {
      await navigator.clipboard.writeText(url.toString());
      const btn = document.getElementById("share");
      const originalText = btn.textContent;
      btn.textContent = "Copied!";
      setTimeout(() => {
        btn.textContent = originalText;
      }, 2000);
    } catch (err) {
      console.error("Failed to copy to clipboard:", err);
      alert("Failed to copy link to clipboard");
    }
  });
}

// Get code from URL hash
function getCodeFromUrl() {
  const hash = window.location.hash.slice(1);
  if (hash.startsWith("code=")) {
    try {
      const encoded = hash.slice(5);
      return decodeURIComponent(atob(encoded));
    } catch (err) {
      console.error("Failed to decode code from URL:", err);
      return null;
    }
  }
  return null;
}

// Save code to localStorage
function saveCodeToStorage() {
  try {
    localStorage.setItem("hebi4-playground-code", getCode());
  } catch (err) {
    console.error("Failed to save to localStorage:", err);
  }
}

// Load code from localStorage
function loadCodeFromStorage() {
  try {
    return localStorage.getItem("hebi4-playground-code");
  } catch (err) {
    console.error("Failed to load from localStorage:", err);
    return null;
  }
}

// Initialize playground
async function init_playground() {
  createEditor();
  setupTabs();
  setupExamples();
  setupRunButton();
  setupOptimizeCheckbox();
  setupShareButton();

  // Show loading state
  document.querySelectorAll(".output").forEach(output => {
    output.textContent = "Loading WASM module...";
    output.className = "output loading";
  });

  await initWasm();
}

// Start the playground
init_playground();
