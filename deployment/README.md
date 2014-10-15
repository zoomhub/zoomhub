### Ansible-Vault

We use the `ansible-vault` to secure sensitive information in our repository. The best place to read about the vault is on the [ansible documentation](http://docs.ansible.com/playbooks_vault.html).

#### Quickstart

Editing the ansible vault is straight-forward.

```bash
$ ansible-vault edit deployment/secrets.vault.yml
```

Saving changes will mark the vault as dirty in your repository, simply `git add` and `git commit` as with any other file. Whenever you run `./zh setup` or `./zh deploy` you'll be prompted for the vault password

#### Local Overrides

If you want to over-ride any ansible variables without having to edit configs, you can put a value in `vars.yml`, which is .gitignored so you can't commit it. This is a convenient way to develop new features before encrypting in the vault.
