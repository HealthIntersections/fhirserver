OPENSSL_DIR=${OPENSSL_DIR:-"/tmp/openssl"}

apt update && apt install -y libgtk2.0-0 


# Check if OpenSSL is not installed
if [ ! -d $OPENSSL_DIR ]; then
    echo "OpenSSL not found in $OPENSSL_DIR. Installing..."
    apt install -y build-essential 

    # Download and build OpenSSL 1.1.1w
    pushd /tmp
    wget https://www.openssl.org/source/openssl-1.1.1w.tar.gz
    tar -xf openssl-1.1.1w.tar.gz
    cd openssl-1.1.1w

    ./config --prefix=$OPENSSL_DIR --openssldir=$OPENSSL_DIR
    make
    make test
    make install
    popd

    echo "OpenSSL installation completed."
else
    echo "OpenSSL is already installed in $OPENSSL_DIR."
fi

    cp $OPENSSL_DIR/lib/*.so* .


