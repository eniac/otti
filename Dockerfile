FROM ubuntu:20.04

ADD . /files

ARG MODEL=models/1afiro.mps
ARG RUST_TOOLCHAIN=nightly-2021-03-24
ENV MODEL=${MODEL}
ENV PATH="$HOME/.cabal/bin:/root/.ghcup/bin:/root/.cargo/bin:${PATH}"
ENV RUSTFLAGS="-C target_cpu=native"
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=0

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
	build-essential curl libffi-dev libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5 \
	git python3 python3-pip python2 python2-dev z3 libz3-dev && \
    pip3 install pysmps numpy && \
    curl https://sh.rustup.rs -sSf | bash -s -- -y && \
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh && \
    rm -rf /var/lib/apt/lists/*

RUN ghcup install stack && \
    rustup toolchain install ${RUST_TOOLCHAIN} && \
    cd /files && \
    git clone --depth 1 --branch v1.0 https://github.com/elefthei/spartan-zkinterface.git && \
    git clone --depth 1 --branch v1.1 https://github.com/eniac/otti.git && \
    tar xvf lp_solve_5.5.2.11_dev_ux64.tar.gz  --one-top-level=dev && \
    tar xvf lp_solve_5.5.2.11_exe_ux64.tar.gz  --one-top-level=exe && \
    tar xvf lp_solve_5.5.2.11_Python_source.tar.gz --one-top-level=Python && \
    cd dev && cp *.h /usr/include/ && cp liblpsolve55.a /usr/lib/ && cp liblpsolve55.so /usr/lib/ && cd .. && \
    cd exe && cp *.so /usr/lib/ && cp lp_solve /usr/bin/ && cd .. && \
    cd Python/lp_solve_5.5/extra/Python && python2 setup.py install

RUN \
    cd /files/compiler && \
    touch empty && \
    python3 /files/cgen.py /files/${MODEL} /files/${MODEL}.c && \
    C_outputs=/files/${MODEL}.zkif stack run --install-ghc -- c main /files/${MODEL}.c --emit-r1cs && \
    C_outputs=/files/${MODEL}.zkif stack run --install-ghc -- c main /files/${MODEL}.c --prove -i empty && \
    cd /files/spartan-zkinterface && \
    rustup override set ${RUST_TOOLCHAIN} && \
    cargo build --release

CMD /files/spartan-zkinterface/target/release/spzk verify --nizk /files/${MODEL}.zkif /files/${MODEL}.inp.zkif /files/${MODEL}.wit.zkif
