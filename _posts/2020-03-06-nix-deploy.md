---
title: nix-deploy, a ligthweight alternative to nixops
description: Have you ever heard about nix-deploy ? This blog is an introduction to this modular and ligthweight alternative to nixops.
categories:
 - nix
tags:
 - nix
 - devops
 - nix-delegate
 - nix-deploy
 - nix-copy-closure
 - nixops
---

Today, I would like to talk about [nix-deploy](http://ixmatus.net/articles/deploy-software-nix-deploy.html).
But, before I'd like to introduce [nix-delegate](https://github.com/awakesecurity/nix-delegate) to have a better understanding of `nix-deploy`.

`nix-deploy` is a safe replacement for `nix-copy-closure` and `nixops`.

# nix-delegate

Runs a nix `command` in a remote machine.

In this example `$ nix-build --no-out-link -A shipit release.nix` is run in the host `jenkins-slave-nixos01`.

```bash
$ nix-delegate --host parnell@jenkins-slave-nixos01 --cores 4 --x86_64-linux --key /home/parnell/.ssh/awake nix-build --no-out-link -A shipit release.nix
[+] Downloading: /etc/nix/signing-key.sec
[+] Installing: /etc/nix/signing-key.sec
[+] Downloading: /etc/nix/signing-key.pub
[+] Installing: /etc/nix/signing-key.pub
[+] Running command: sudo nix-build --no-out-link -A shipit release.nix
[+] Full command context: sudo NIX_BUILD_HOOK=/nix/store/jj3kq2dmllvkqwwbhnmzbk9hfgncdbvl-nix-1.11.6/libexec/nix/build-remote.pl
...
/nix/store/wwrrkwzhq43c22if31d65qikslmvivyc-shipit-1.0.0
```

# nix-deploy

- Generate a pair of signing keys on the remote computer if none exist.
- Exchange the signing keys with the local computer.
- Deploy the closure using `nix-copy-closure`.
- If the closure is a system configuration, activate it on the remote computer using `switch-to-configuration`.

More information about `nix-copy-closure` at [appendix A](#appendix-a).

Motivation:

- `nix-copy-closure` doesn't exchange signing keys which is insecure (`nix-deploy path`).
- lightweight `nixops` (`nix-deploy system`).

## nix-deploy path

`nix-deploy path`: generates signing keys, exchange them and runs `nix-copy-closure`.

Securely deploy `hello` build product to `remote-host`:

```bash
$ nix-instantiate '<nixpkgs>' --attr hello
# /nix/store/yx6vm61402bxfpx7z3yxq7r1zmv7cqmy-hello-2.10.drv
$ nix-store --realise /nix/store/yx6vm61402bxfpx7z3yxq7r1zmv7cqmy-hello-2.10.drv
# /nix/store/1y6ckg6khrdsvll54s5spcmf3w6ka9k4-hello-2.10
$ nix-deploy path --to parnell@remote-host --path /nix/store/1y6ckg6khrdsvll54s5spcmf3w6ka9k4-hello-2.10
# [+] Downloading: /etc/nix/signing-key.sec
# [+] Installing: /etc/nix/signing-key.sec
#
#     This will prompt you for your `sudo` password
# [sudo] password for parnell:
# [+] Downloading: /etc/nix/signing-key.pub
# [+] Installing: /etc/nix/signing-key.pub
# [+] Copying /nix/store/1y6ckg6khrdsvll54s5spcmf3w6ka9k4-hello-2.10
#
# copying 2 missing paths (20.00 MiB) to ‘parnell@remote-host’...
```

If everything goes well:

```bash
$ ssh parnell@remote-host /nix/store/1y6ckg6khrdsvll54s5spcmf3w6ka9k4-hello-2.10/bin/hello
# Hello, world!
```

## nix-deploy system

`nixops`  does three things:

1. instantiate and manage resources that will host and run the system (which contains your software).
2. build the system configuration and software defined in the deployment specification.
3. deploy the system and activate it.

A _system configuration_ is a bootable NixOS system configuration (example on [Appendix B](#appendix-b)).

`nix-deploy system` simply copies the closure to the target computer, and then executes the `switch-to-configuration` script (same mechanism used by `nixos-rebuild switch`) found within the build product of the system configuration it deployed.

Finally, we can test the activation of a system configuration via the `--dry-activate` option:

```bash
$ nix-deploy system --to parnell@remote-host --dry-activate --path /nix/store/vrg7l1zxih48m2k88fdg1byld72lrjcg-nixos-system-unnamed-17.09.2182

# these paths will be fetched (0.97 MiB download, 3.19 MiB unpacked):
#   /nix/store/3canvs63nkqcqiqiv6mzj0j5g4rawb44-libressl-2.5.5-bin
#   /nix/store/c1xj32krgc7d6sc6pqzzkcgxyb5a16sd-libressl-2.5.5
# ...
# [+] Installing system: /nix/store/vrg7l1zxih48m2k88fdg1byld72lrjcg-nixos-system-unnamed-17.09.2182
#
# copying 412 missing paths (523.49 MiB) to ‘34.201.68.87’
# ...
```

# Credits

This blog was heavily inspired by _"[Deploy software easily and securely using nix-deploy](http://ixmatus.net/articles/deploy-software-nix-deploy.html)"_.

# References

- nix-delegate, <https://github.com/awakesecurity/nix-delegate>
- nix-deploy, <https://github.com/awakesecurity/nix-deploy>
- nix-copy-closure, <https://nixos.org/nix/manual/#name-6>
- nixops, <https://nixos.org/nixops/>
- Deploy software easily and securely using nix-deploy, <http://ixmatus.net/articles/deploy-software-nix-deploy.html>

# Appendix A

## nix-copy-closure

Copying __closure of a path__ in a nix store to another nix store.

A __path__ can be:

- A __derivation instantiated__:

```bash
$ nix-instantiate '<nixpkgs>' --attr hello
# /nix/store/dhmin7wq99aw9f59jm41varj0753va9b-hello-2.10.drv
```

- A __build product__ from the evaluation of a derivation:

```bash
$ nix-instantiate '<nixpkgs>' --attr hello
# /nix/store/dhmin7wq99aw9f59jm41varj0753va9b-hello-2.10.drv
$ nix-store --realise /nix/store/dhmin7wq99aw9f59jm41varj0753va9b-hello-2.10.drv
# /nix/store/4w99qz14nsahk0s798a5rw5l7qk1zwwf-hello-2.10
```

A _closure of a path_ in the nix store is the __graph__ of that path's dependencies.

- Closure of a derivation:

```bash
$ nix-instantiate '<nixpkgs>' --attr hello
# /nix/store/dhmin7wq99aw9f59jm41varj0753va9b-hello-2.10.drv
$ nix-store --query --requisites /nix/store/dhmin7wq99aw9f59jm41varj0753va9b-hello-2.10.drv
# /nix/store/01n3wxxw29wj2pkjqimmmjzv7pihzmd7-which-2.21.tar.gz.drv
# /nix/store/064jmylcq7h6fa5asg0rna9awcqz3765-0001-x86-Properly-merge-GNU_PROPERTY_X86_ISA_1_USED.patch
# /nix/store/0g93706i7g54hwilxkd9lhyfmmwy4jr6-openssl-1.1.1d.tar.gz.drv
```

- Closure of a build product:

```bash
$ nix-instantiate '<nixpkgs>' --attr hello
# /nix/store/dhmin7wq99aw9f59jm41varj0753va9b-hello-2.10.drv
$ nix-store --realise /nix/store/dhmin7wq99aw9f59jm41varj0753va9b-hello-2.10.drv
# /nix/store/4w99qz14nsahk0s798a5rw5l7qk1zwwf-hello-2.10
$ nix-store --query --requisites /nix/store/dhmin7wq99aw9f59jm41varj0753va9b-hello-2.10.drv
# /nix/store/wx1vk75bpdr65g6xwxbj4rw0pk04v5j3-glibc-2.27
# /nix/store/4w99qz14nsahk0s798a5rw5l7qk1zwwf-hello-2.10
```

There are two types of deployment in Nix:

- __source deployment__ (copying the closure of a nix store path that is a derivation)
- __binary deployment__ (copying the closure of a nix store path that is a build product)

`nix-copy-closure` deploys both types (assuming the target computer has `nix` installed and _SSH credentials_ configured.)

Example of _source deployment_/_binary deployment_ of the `hello` package to the computer addresed at `remote-host` and user `parnell` configured with working SSH credentials:

```bash
#### source deployment

$ nix-instantiate '<nixpkgs>' --attr hello
# /nix/store/yx6vm61402bxfpx7z3yxq7r1zmv7cqmy-hello-2.10.drv
$ nix-copy-closure --to parnell@remote-host /nix/store/yx6vm61402bxfpx7z3yxq7r1zmv7cqmy-hello-2.10.drv

###################################

#### binary deployment

$ nix-instantiate '<nixpkgs>' --attr hello
# /nix/store/dhmin7wq99aw9f59jm41varj0753va9b-hello-2.10.drv
$ nix-store --realise /nix/store/dhmin7wq99aw9f59jm41varj0753va9b-hello-2.10.drv
# /nix/store/4w99qz14nsahk0s798a5rw5l7qk1zwwf-hello-2.10
$ nix-copy-closure --to parnell@remote-host /nix/store/4w99qz14nsahk0s798a5rw5l7qk1zwwf-hello-2.10
```

# Appendix B

A system configuration is a bootable NixOS-based computer operating system with a kernel, filesystem specification, init system (both default init scripts and user-specific init scripts), and user-land software.

Minimal system configuration for an EC2 machine (deployable to an EC2 instance):

```nix
let
  nixos-ec2 = import <nixpkgs/nixos> {
    system = "x86_64-linux";

    configuration = {
      imports = [
        <nixpkgs/nixos/modules/virtualisation/amazon-image.nix>
      ];

      ec2.hvm = true;

      users.users.parnell = {
        group       = "users";
        extraGroups = [
          "wheel" "disk" "audio" "video" "vboxusers"
          "networkmanager" "systemd-journal"
        ];
      };
    };
  };
in
  nixos-ec2.system
```

which can be instantiate:

```bash
$ nix-instantiate minimal-ec2-nixos.nix
# /nix/store/y3aalvgw4v62f5w0hy2vlaz91ynmp3kf-nixos-system-unnamed-17.09.2182.drv
```

and build:

```bash
$ nix-store --realise /nix/store/bfzijb8xsppldrj814hkvj7swij6xjd9-nixos-system-nixos-17.09.2182.drv
# these derivations will be built:
#   /nix/store/10lmxlmrbvr7k26l133jr1mwjxjfv74y-etc-nixos.conf.drv
#   /nix/store/h7jc20dvz1qk3h3jxx0wwp7p9nlkcp7p-grub-config.xml.drv
#   /nix/store/1m56czmckmmy4mwpgsisqiff8wc1cijq-users-groups.json.drv
#   /nix/store/f25wpc46yjlk30h0im8gwmni94ahpmvf-system-path.drv
# ...
# /nix/store/vrg7l1zxih48m2k88fdg1byld72lrjcg-nixos-system-unnamed-17.09.2182
```

We can see that the build product of `/nix/store/bfzijb8xsppldrj814hkvj7swij6xjd9-nixos-system-nixos-17.09.2182.drv` is a NixOS Linux system configuration with a kernel, init ramdisk image, and a systemd init system:

```bash
$ tree /nix/store/vrg7l1zxih48m2k88fdg1byld72lrjcg-nixos-system-unnamed-17.09.2182
# /nix/store/vrg7l1zxih48m2k88fdg1byld72lrjcg-nixos-system-unnamed-17.09.2182
# ├── activate
# ├── bin
# │   └── switch-to-configuration
# ├── configuration-name
# ├── etc -> /nix/store/cagpxljdhmrsrgwjwiq1q5y2jv28pyfv-etc/etc
# ├── extra-dependencies
# ├── fine-tune
# ├── firmware -> /nix/store/337bpg5m7ynry8yc0wmmwwdp8bpdqg7d-firmware/lib/firmware
# ├── init
# ├── init-interface-version
# ├── initrd -> /nix/store/d4j5awvlbzplzq0jl4bhxy1j46ggy7f6-initrd/initrd
# ├── kernel -> /nix/store/gi2bg1sdibi0d1s692cgf8k5h2p20a95-linux-4.9.65/bzImage
# ├── kernel-modules -> /nix/store/jiqq02yxvi637s3zrfjzmlyxiddiwk8j-kernel-modules
# ├── kernel-params
# ├── nixos-version
# ├── sw -> /nix/store/03fkz4ck643zx23ag5i45pgnlzqrm9b6-system-path
# ├── system
# └── systemd -> /nix/store/cd2r3b7j655vfdnvfwci71dn4yyaxa0p-systemd-234
#
# 7 directories, 12 files
```

... thus, we can deploy nixos-system-unnamed-17.09.2182 and all of its runtime dependencies!
