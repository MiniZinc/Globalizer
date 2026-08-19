#!/usr/bin/env bash
# Build Globalizer and lay it out the way the IDE bundle expects: the binary in
# bin/, its mznlib and solver configuration under share/minizinc/.
#
# Env: ROOT, STACK_ROOT (kept outside the workspace in containers; stack refuses
# to create a root whose parent belongs to another user)
set -eux

ROOT="${ROOT:-$PWD}"
cd "$ROOT"

# Containers ship neither stack nor the libraries GHC links against: GMP for
# ghc-bignum, ncurses for the RTS's terminal support.
if command -v apk >/dev/null; then
  # The extra packages are what ghcup needs to install GHC on Alpine.
  apk add --no-cache bash curl tar xz make g++ gmp-dev ncurses-dev \
    binutils libc-dev libffi-dev perl zlib-dev >/dev/null
elif command -v dnf >/dev/null; then
  dnf -y install gmp-devel ncurses-compat-libs perl xz >/dev/null
fi

# downloads.haskell.org 503s often enough to have cost a release build.
retry() {
	for attempt in 1 2 3 4 5; do
		"$@" && return 0
		echo "attempt $attempt of '$*' failed; retrying in $((attempt * 15))s" >&2
		sleep $((attempt * 15))
	done
	echo "'$*' still failing after 5 attempts" >&2
	return 1
}

# stack serves aarch64 Linux by neither channel -- no ARM64 installer, and no
# aarch64 GHC bindists -- so ghcup provides both there and stack just drives it.
GHC_ARGS=(--install-ghc)
if [ "$(uname -s)" = Linux ] && [ "$(uname -m)" = aarch64 ]; then
  export GHCUP_INSTALL_BASE_PREFIX="${STACK_ROOT:-$HOME/.stack}"
  export BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1
  # The failing request is inside the bootstrap script, so the retry has to wrap
  # the whole thing rather than any curl here.
  command -v ghcup >/dev/null || retry sh -c 'curl -sSf https://get-ghcup.haskell.org | sh'
  export PATH="$GHCUP_INSTALL_BASE_PREFIX/.ghcup/bin:$PATH"
  retry ghcup install ghc 9.6.7 --set
  retry ghcup install stack latest --set
  GHC_ARGS=(--system-ghc --no-install-ghc)
else
  command -v stack >/dev/null || retry sh -c 'curl -sSL https://get.haskellstack.org/ | sh -s - -f'
fi

mkdir -p globalizer/bin globalizer/share/minizinc/solvers
# --allow-different-user: in a container we are root while the mounted workspace
# belongs to the runner user. The flag is ignored on Windows.
stack --allow-different-user --local-bin-path "$ROOT/globalizer/bin" \
  build "${GHC_ARGS[@]}" --copy-bins
cp -r mznlib/globalizer globalizer/share/minizinc/
cp globalizer.msc globalizer/share/minizinc/solvers/
