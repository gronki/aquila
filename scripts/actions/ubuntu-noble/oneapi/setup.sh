apt-get update && apt-get install -y gpg gpg-agent wget
wget -O- https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB \
    | gpg --dearmor | tee /usr/share/keyrings/oneapi-archive-keyring.gpg
echo "deb [signed-by=/usr/share/keyrings/oneapi-archive-keyring.gpg] " \
    " https://apt.repos.intel.com/oneapi all main" \
    | tee /etc/apt/sources.list.d/oneAPI.list
apt-get update && apt-get install -y --no-install-recommends \
    intel-oneapi-compiler-fortran-2025.2 \
    intel-oneapi-compiler-dpcpp-cpp-2025.2