// @ts-check

import {
  EditorView, lineNumbers, highlightActiveLineGutter, highlightSpecialChars,
  drawSelection, dropCursor, rectangularSelection, crosshairCursor,
  highlightActiveLine, keymap
} from "@codemirror/view";
import { EditorState } from "@codemirror/state";
import {
  foldGutter, indentOnInput, syntaxHighlighting, defaultHighlightStyle,
  bracketMatching, foldKeymap
} from "@codemirror/language";
import { history, historyKeymap, defaultKeymap } from "@codemirror/commands";
import { searchKeymap, highlightSelectionMatches } from "@codemirror/search";
import { autocompletion, completionKeymap, closeBrackets, closeBracketsKeymap } from "@codemirror/autocomplete";
import { lintKeymap } from "@codemirror/lint";
import { examples } from "./examples.js";
import init, { parse, disassemble, run } from "./pkg/hebi4_playground.js";

/** @type {EditorView} */
let editor;
let wasmReady = false;

async function initWasm() {
  try {
    await init();
    wasmReady = true;
    console.log("Hebi4 Wasm initialized");
    updateAllOutputs();
  } catch (err) {
    console.error("Failed to initialize Wasm:", err);
    showError("run", "Failed to initialize Wasm module");
  }
}

function createEditor() {
  const defaultCode = getCodeFromUrl() || loadCodeFromStorage() || examples.welcome.code;

  let updateTimer;

  const runCommand = (view) => {
    updateAllOutputs();
    return true;
  };

  const state = EditorState.create({
    doc: defaultCode,
    extensions: [
      lineNumbers(),
      highlightActiveLineGutter(),
      highlightSpecialChars(),
      history(),
      foldGutter(),
      drawSelection(),
      dropCursor(),
      EditorState.allowMultipleSelections.of(true),
      indentOnInput(),
      syntaxHighlighting(defaultHighlightStyle, { fallback: true }),
      bracketMatching(),
      closeBrackets(),
      autocompletion(),
      rectangularSelection(),
      crosshairCursor(),
      highlightActiveLine(),
      highlightSelectionMatches(),
      keymap.of([
        { key: "Ctrl-Enter", run: runCommand },
        { key: "Cmd-Enter", run: runCommand },
        ...closeBracketsKeymap,
        ...defaultKeymap,
        ...searchKeymap,
        ...historyKeymap,
        ...foldKeymap,
        ...completionKeymap,
        ...lintKeymap
      ]),
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
      EditorView.updateListener.of((update) => {
        if (update.docChanged) {
          clearTimeout(updateTimer);
          updateTimer = setTimeout(() => {
            saveCodeToStorage();
          }, 1000);
        }
      })
    ]
  });

  editor = new EditorView({
    state,
    parent: document.getElementById("editor") ?? undefined
  });
}

function getCode() {
  return editor.state.doc.toString();
}

function getOptimize() {
  return /** @type {HTMLInputElement} */(document.getElementById("optimize"))?.checked;
}

function updateAllOutputs() {
  if (!wasmReady) {
    return;
  }

  const code = getCode();
  const optimize = getOptimize();
  updateAst(code);
  updateDisasm(code, optimize);
  updateRun(code, optimize);
}

/**
 * @param {string} code
 */
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

/**
 * @param {string} code
 * @param {boolean} optimize
 */
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

/**
 * @param {string} code
 * @param {boolean} optimize
 */
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

/**
 * @param {string} tab
 * @param {string} message
 */
function showError(tab, message) {
  const output = document.getElementById(`output-${tab}`);
  output.textContent = message;
  output.className = "output error";
  if (getCurrentTab() === tab) {
    output.classList.add("active");
  }
}

function getCurrentTab() {
  const activeTab = /** @type {HTMLElement} */(document.querySelector(".tab.active"));
  return activeTab ? activeTab.dataset.tab : "run";
}

function setupTabs() {
  const tabs = document.querySelectorAll(".tab");
  const outputs = document.querySelectorAll(".output");

  tabs.forEach(tab => {
    tab.addEventListener("click", () => {
      tabs.forEach(t => t.classList.remove("active"));
      outputs.forEach(o => o.classList.remove("active"));

      tab.classList.add("active");
      const tabName = /** @type {HTMLElement} */(tab).dataset.tab;
      document.getElementById(`output-${tabName}`).classList.add("active");
    });
  });
}

function setupExamples() {
  const select = document.getElementById("examples");

  Object.entries(examples).forEach(([key, example]) => {
    const option = document.createElement("option");
    option.value = key;
    option.textContent = example.name;
    select.appendChild(option);
  });

  select.addEventListener("change", (e) => {
    const target = /** @type {HTMLSelectElement} */(e.target);
    const exampleKey = target.value;
    if (exampleKey && examples[exampleKey]) {
      setCode(examples[exampleKey].code);
      updateAllOutputs();
    }
    target.value = "";
  });
}

/**
 * @param {string} code
 */
function setCode(code) {
  editor.dispatch({
    changes: {
      from: 0,
      to: editor.state.doc.length,
      insert: code
    }
  });
}

function setupRunButton() {
  document.getElementById("run").addEventListener("click", () => {
    updateAllOutputs();
  });
}

function setupOptimizeCheckbox() {
  document.getElementById("optimize").addEventListener("change", () => {
    updateAllOutputs();
  });
}

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

function saveCodeToStorage() {
  try {
    localStorage.setItem("hebi4-playground-code", getCode());
  } catch (err) {
    console.error("Failed to save to localStorage:", err);
  }
}

function loadCodeFromStorage() {
  try {
    return localStorage.getItem("hebi4-playground-code");
  } catch (err) {
    console.error("Failed to load from localStorage:", err);
    return null;
  }
}

async function init_playground() {
  createEditor();
  setupTabs();
  setupExamples();
  setupRunButton();
  setupOptimizeCheckbox();
  setupShareButton();

  document.querySelectorAll(".output").forEach(output => {
    output.textContent = "Loading WASM module...";
    output.className = "output loading";
  });

  await initWasm();
}

init_playground();
