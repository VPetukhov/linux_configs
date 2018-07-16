FROM library/centos:7.4.1708
LABEL maintainer="viktor.s.petuhov@ya.ru"

RUN yum -y update

RUN \
  yum -y install epel-release && \
  yum -y install \
    cmake3 \
    git \
    htop \
    ImageMagick \
    libcurl-devel \
    openssl-devel \
    R \
    sudo \
    tmux \
    vim \
    wget \
    zsh

RUN ln -s /usr/bin/cmake3 /usr/bin/cmake

# gcc
RUN \
  yum -y install centos-release-scl && \
  yum -y install devtoolset-7-gcc*

# Python
RUN source /opt/rh/devtoolset-7/enable && \
  yum install -y https://centos7.iuscommunity.org/ius-release.rpm && \
  yum install -y python36u python36u-libs python36u-devel python36u-pip


RUN \
  ln -s /bin/python3.6 /bin/python3 && \
  ln -s /bin/pip3.6 /bin/pip3

RUN pip3 install jupyter numpy scipy matplotlib seaborn pandas sklearn scikit-image \
  jupyter_contrib_nbextensions jupyter_nbextensions_configurator jupyterthemes

RUN \
  jupyter nbextensions_configurator enable && \
  jupyter contrib nbextension install

# Julia
RUN yum install -y bzip2 hdf5 m4 openssl patch

RUN \
  wget https://github.com/JuliaLang/julia/releases/download/v0.6.4/julia-0.6.4-full.tar.gz && \
  tar xvf julia-0.6.4-full.tar.gz

 RUN \
  source /opt/rh/devtoolset-7/enable && \
  cd julia && \
  make -j 40 && \
  make -j 40 install

RUN \
  ln -s /julia/usr/bin/julia /bin/julia

# User
RUN useradd -m -s /bin/zsh -G wheel -u 1029 vp && \
  passwd -d vp

USER vp

ENTRYPOINT ["/bin/zsh"]
WORKDIR /home/vp

RUN \
  jupyter nbextension enable codefolding/main && \
  jupyter nbextension enable codefolding/edit && \
  jupyter nbextension enable collapsible_headings/main && \
  # jupyter nbextension enable code_prettify/isort && \
  jupyter nbextension enable notify/notify && \
  jupyter nbextension enable ruler/main && \
  jupyter nbextension enable table_beautifier/main && \
  jupyter nbextension enable comment-uncomment/main && \
  jupyter nbextension enable hide_header/main && \
  jupyter nbextension enable livemdpreview/livemdpreview && \
  jupyter nbextension enable spellchecker/main && \
  jupyter nbextension enable execution_dependencies/execution_dependencies && \
  jupyter nbextension enable freeze/main && \
  jupyter nbextension enable tree-filter/index && \
  jupyter nbextension enable init_cell/main && \
  jupyter nbextension enable move_selected_cells/main && \
  jupyter nbextension enable toc2/main

RUN git clone https://github.com/VPetukhov/linux_configs.git && \
  echo "" > .bashrc && \
  cp linux_configs/.zshrc . && \
  cp linux_configs/.bash_aliases . && \
  git clone https://github.com/robbyrussell/oh-my-zsh.git .oh-my-zsh && \
  mkdir -p .jupyter/custom/ && \ 
  cp linux_configs/jupyter_theme.css ~/.jupyter/custom/custom.css

RUN echo '{"NotebookApp": {"password": "sha1:40e75814693d:13a7813e89658790bd76440f5a70506c8e37c9f9"}}' > .jupyter/jupyter_notebook_config.json

RUN echo 'for pkg in ["DataFrames", "Plots", "Distributions", "StatsBase", "HDF5", "Suppressor", "RCall", "PyCall", "FileIO", "CSV", "PyPlot", "StatsBase", "Iterators", "Suppressor", "NearestNeighbors", "ProgressMeter", "Distances", "Clustering", "MultivariateStats", "ConcaveHull", "Graphs", "IterTools", "ImageSegmentation", "IJulia", "PlotlyJS", "TSne", "ImageMagick"] Pkg.add(pkg) end; Pkg.build()' > julia_pkgs.jl

ENV PYTHON=/usr/bin/python3
RUN julia julia_pkgs.jl
