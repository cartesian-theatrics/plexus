#!/usr/bin/env python3
"""Regression-check the exported hooks using clj-kondo on PATH (or CLJ_KONDO)."""
import json
import os
import subprocess

config = "resources/clj-kondo.exports/org.clojars.cartesiantheatrics/plexus"
for fixture, expected in [("valid", 0), ("invalid", 12)]:
    result = subprocess.run(
        [os.environ.get("CLJ_KONDO", "clj-kondo"), "--lint", f"test/lint/{fixture}.clj",
         "--config-dir", config, "--config", "{:output {:format :json}}"],
        text=True, capture_output=True)
    report = json.loads(result.stdout)
    findings = report["findings"]
    assert len(findings) == expected, (fixture, findings, result.stderr)
    assert all(f["type"] == "plexus/options" for f in findings), findings
    assert result.returncode == (3 if expected else 0), result
print("Static checks passed: valid source accepted; 12 invalid-option findings located.")
