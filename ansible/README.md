# Ansible configs

To use these configs you need to clone this repository and enter this directory (`linux_configs/ansible`).

## Install Ansible on the master machine

You can either use pip or conda.

Pip:
```bash
pip install --user ansible
```

Conda (environment name is `ansible`):
```bash
mamba env create -f ./env.yml
```

## Initial configuration of the target machine

If it only has a root user, create a new user with sudo privileges:
```bash
sudo useradd -m centos -G wheel
```

[Disable root and password login](https://www.cyberciti.biz/faq/how-to-disable-ssh-password-login-on-linux/) by editing `/etc/ssh/sshd_config`:
```
PermitRootLogin no
PasswordAuthentication no
```

Then: `sudo systemctl reload sshd`.

Also, allow `centos` to run sudo commands without password: `visudo` and set `%wheel  ALL=(ALL)       NOPASSWD: ALL`.

## Install software

To install the software through Ansible, you first need to fill in the `inventory.ini` file. Example content:
```ini
[centos_base]
ipv4.address     ansible_connection=ssh        ansible_user=username
```

Don't forget to set the `host` variable in the `.yml` files to match the group name in the `inventory.ini` file (eg, `centos_base`).

Then, you can run the following command:

```bash
ansible-playbook --inventory ./inventory.ini init_centos.yml # use init_ubuntu.yml for Ubuntu
ansible-playbook --inventory ./inventory.ini user_cross_platform.yml
```