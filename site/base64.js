/**
 * @param {Uint8Array} bytes
 */
function bytesToBase64(bytes) {
  let s = "";
  const chunk = 0x8000; // avoid call stack / arg limits
  for (let i = 0; i < bytes.length; i += chunk) {
    s += String.fromCharCode.apply(null, bytes.subarray(i, i + chunk));
  }
  return btoa(s);
}

/**
 * @param {string} b64
 */
function base64ToBytes(b64) {
  const bin = atob(b64);
  const bytes = new Uint8Array(bin.length);
  for (let i = 0; i < bin.length; i++) bytes[i] = bin.charCodeAt(i);
  return bytes;
}

// ---- Stream helpers ----
/**
 * @param {Uint8Array<ArrayBuffer>} bytes
 * @param {CompressionFormat} format
 * @returns {Promise<{ bytes: Uint8Array<ArrayBuffer>, compressed: boolean }>}
 */
async function compressBytes(bytes, format = "gzip") {
  if (!("CompressionStream" in self)) return { bytes, compressed: false };
  const cs = new CompressionStream(format);
  const writer = cs.writable.getWriter();
  writer.write(bytes);
  writer.close();
  const buf = await new Response(cs.readable).arrayBuffer();
  return { bytes: new Uint8Array(buf), compressed: true };
}

/**
 * @param {Uint8Array<ArrayBuffer>} bytes
 * @param {CompressionFormat} format
 * @returns {Promise<{ bytes: Uint8Array<ArrayBuffer>, decompressed: boolean }>}
 */
async function decompressBytes(bytes, format = "gzip") {
  if (!("DecompressionStream" in self)) return { bytes, decompressed: false };
  const ds = new DecompressionStream(format);
  const writer = ds.writable.getWriter();
  writer.write(bytes);
  writer.close();
  const buf = await new Response(ds.readable).arrayBuffer();
  return { bytes: new Uint8Array(buf), decompressed: true };
}

// ---- Public API ----
// Compress a string and encode as Base64 (gzip when supported).
// Returns a string with a small header to indicate whether it’s compressed.

/**
 * @param {string} str
 */
export async function encode(str) {
  const src = new TextEncoder().encode(str);
  const { bytes, compressed } = await compressBytes(src, "gzip");
  // Header "G:" means gzip (or chosen format) was used; "N:" means no compression fallback
  const header = compressed ? "G:" : "N:";
  return header + bytesToBase64(bytes);
}

// Decode Base64 and decompress back to the original string.
/**
 * @param {string} b64
 */
export async function decode(b64) {
  const header = b64.slice(0, 2);
  const payload = b64.slice(2);
  const bytes = base64ToBytes(payload);

  let out;
  if (header === "G:") {
    // Expect compressed payload; if the API isn't available, throw a clear error
    if (!("DecompressionStream" in self)) {
      throw new Error("This content is gzip-compressed but DecompressionStream isn't supported in this browser.");
    }
    const { bytes: dec } = await decompressBytes(bytes, "gzip");
    out = dec;
  } else if (header === "N:") {
    // No-compression fallback payload
    out = bytes;
  } else {
    // Backward-compat: if no header present, try to decompress first, then fall back.
    const tryBytes = base64ToBytes(b64);
    if ("DecompressionStream" in self) {
      try {
        const { bytes: dec } = await decompressBytes(tryBytes, "gzip");
        out = dec;
      } catch {
        out = tryBytes;
      }
    } else {
      out = tryBytes;
    }
  }

  return new TextDecoder().decode(out);
}
