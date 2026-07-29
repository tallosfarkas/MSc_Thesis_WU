#!/usr/bin/env bash
CLUSTER="${CLUSTER:-wucluster}"   # override with your own SSH host alias
# ==============================================================================
# pipeline/cluster/preflight.sh
# Pre-flight checks before submitting to the cluster (called by `make submit-*`):
#   1. no uncommitted changes (the cluster runs committed code)
#   2. no secrets in tracked code (prevents the credential-leak class of incident)
#   3. cluster reachable (VPN up)
# Exits non-zero on any failure so `make` halts before submitting.
# ==============================================================================
set -uo pipefail
ROOT="$(cd "$(dirname "$0")/../.." && pwd)"; cd "$ROOT"
fail=0
echo "== preflight =="

# 1. uncommitted changes
if [ -n "$(git status --porcelain 2>/dev/null)" ]; then
  echo "  [!] uncommitted changes — commit first (cluster runs committed code):"
  git status --short | head
  fail=1
else
  echo "  [ok] git working tree clean"
fi

# 2. secret scan over tracked code
# Match hardcoded LITERAL secrets (quoted strings / known token shapes), not
# variable references like `password = wrds_pass` (value from env/keyring).
SECRET_RE='mongodb(\+srv)?://[^[:space:]]*:[^[:space:]@]+@|-----BEGIN [A-Z ]*PRIVATE KEY-----|AKIA[0-9A-Z]{16}|(api|secret|access)[_-]?(key|token)[[:space:]]*[:=][[:space:]]*"[A-Za-z0-9/+_-]{16,}"|(password|passwd|pwd)[[:space:]]*[:=][[:space:]]*"[^"]{6,}"'
HITS=$(grep -RInE "$SECRET_RE" \
        --include='*.R' --include='*.py' --include='*.sh' \
        --include='*.yml' --include='*.yaml' --include='*.md' \
        pipeline/ src/ scripts/ 2>/dev/null \
        | grep -viE 'example|placeholder|your_|<[a-z_]+>|dummy|todo|getenv|getoption|keyring|askpass|_pass[,)]|prompt')
if [ -n "$HITS" ]; then
  echo "  [!] possible secret(s) detected — do NOT submit/push:"; echo "$HITS" | head
  fail=1
else
  echo "  [ok] no secrets found in tracked code"
fi

# 3. cluster reachable
if ssh -o ConnectTimeout=8 ${CLUSTER} 'echo ok' >/dev/null 2>&1; then
  echo "  [ok] cluster reachable"
else
  echo "  [!] cluster unreachable (VPN down?)"
  fail=1
fi

if [ "$fail" -eq 0 ]; then echo "PREFLIGHT PASS"; else echo "PREFLIGHT FAIL"; exit 1; fi
