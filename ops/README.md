# Ops

### Prerequisites

-   Install [Ansible]: `brew install ansible`.
    We have tested our setup with Ansible 1.9.4.

### Commands

`./zh` is a script for managing ZoomHub. We support the following commands:

-   `./zh ops bootstrap [admin|production|staging]`:
    Bootstrap admin user on server(s).
    **IMPORTANT:** Can only be run once per server!
-   `./zh ops setup-admin-server`: Set up admin server.
-   `./zh ops setup-web-server [production|staging]`: Set up web server(s).
-   `./zh ops ping [admin|production|staging]`: Ping servers.

### Manual Steps

- Mount block storage volume:
  https://support.rackspace.com/how-to/prepare-your-cloud-block-storage-volume/

## Ansible Vault

We use `ansible-vault` to secure sensitive information in our repository.
The best place to read about the `ansible-vault` is on the
[Ansible documentation](http://docs.ansible.com/playbooks_vault.html).

### Quickstart

Editing the ansible vault is straight-forward.

```bash
ansible-vault edit ops/secrets.vault.yml
```

Saving changes will mark the vault as dirty in your repository, simply `git add`
and `git commit` as with any other file. Whenever you run `./zh setup` or
`./zh deploy` you'll be prompted for the vault password

### Local Overrides

If you want to over-ride any ansible variables without having to edit configs,
you can put a value in `vars.yml`, which is .gitignored so you can't commit it.
This is a convenient way to develop new features before encrypting in the vault.


[Ansible]: http://docs.ansible.com
