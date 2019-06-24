# Utils
print_error() {
    printf "\033[31m    [×] $1\033[m\n"
}

print_success() {
    printf "\033[32m    [✓] $1\033[m\n"
}

print_warning() {
    printf "\033[33m    [!] $1\033[m\n"
}

print_title() {
    printf "\n\n\033[35m$1\033[m\n\n"
}

print_message() {
    printf "    $1\n"
}

# Install
install_MeCab() {
    print_title "MeCab"
    print_message "Installing MeCab..."
    wget -O mecab-0.996.tar.gz "https://drive.google.com/uc?export=download&id=0B4y35FiV1wh7cENtOXlicTFaRUE"
    tar zxfv mecab-0.996.tar.gz
    cd mecab-0.996
    ./configure
    make
    make check
    sudo make install

    print_message "Installing IPA dictionary..."
    wget -O mecab-ipadic-2.7.0-20070801.tar.gz "https://drive.google.com/uc?export=download&id=0B4y35FiV1wh7MWVlSDBCSXZMTXM"
    tar -xzf mecab-ipadic-2.7.0-20070801.tar.gz
    cd mecab-ipadic-2.7.0-20070801; ./configure --with-charset=utf8; make; make install

    print_success "MeCab and IPA dic: successfully installed"
}

install_MeCab
