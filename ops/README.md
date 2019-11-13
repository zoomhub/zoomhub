# Ops

The `./zh ops` subcommands let you manage ZoomHub ops.

### Prerequisites

-   Install [Ansible]: `brew install ansible`.
    We have tested our setup with Ansible 2.9.0.
-   Run `./zh ops setup`.

## Initial Server Setup

-   Create server on Rackspace using Ubuntu 18.04 LTS (Bionic Beaver) image:
    <https://mycloud.rackspace.com/>
-   Note `root` password. You will need it in subsequent steps.
-   **Optional:** Create DNS entry for server.
-   Add hostname or IP to one of the hosts files: `ops/admin`,
    `ops/staging`, or `ops/production`.
-   Create `admin` user and bootstrap server using:
    `./zh ops bootstrap [admin|production|staging]`.
    **IMPORTANT:** This command can only be run once per server!
-   Run `./zh ops ping [admin|production|staging]` to test whether you can reach
    your new server.

## Web servers

Run `./zh ops setup-web-server [production|staging]` and follow the steps to
set up a web server.

### Update app configuration

To quickly update Keter application configuration, e.g. `PROCESS_CONTENT`,
edit `ops/roles/keter/templates/keter-config.yaml.j2` and run:

```
./zh ops setup-web-server [production|staging] --tags 'app-configuration'
```

## Database server

Run `./zh ops setup-database-server [production|staging]` and follow the steps
to set up a database server.

### Debug database remotely

```
ssh -L 3333:<db-server-private-ip>:5432 <user>@<jumpbox>
psql --host=localhost --port=3333 --username=<db-user> --dbname=<db-name>
```

## Admin server

Run `./zh ops setup-admin-server` and follow the steps to set up an admin server.

## Manual Steps

- Mount block storage volume:
  https://support.rackspace.com/how-to/prepare-your-cloud-block-storage-volume/

- Create SSH key entry for each target deploy host in CircleCI using
  `circleci_deploy.id_rsa` private key:
  <https://circleci.com/gh/gasi/zoomhub/edit#ssh>

- Add new server to Splunk:
  1. <http://admin.zoomhub.net:8000/en-US/manager/search/adddata/selectforwarders>
  2. Watch file: `/opt/keter/log/app-zoomhub/current.log`

## Ansible Vault

We use `ansible-vault` to secure sensitive information in our repository.
The best place to read about the `ansible-vault` is on the
[Ansible documentation](http://docs.ansible.com/playbooks_vault.html).

### Quickstart

Editing the Ansible vault is straightforward using:
`ansible-vault edit ops/secrets.vault.yml`.

Saving changes will mark the vault as dirty in your repository, simply `git add`
and `git commit` as with any other file. Whenever you run any of the `./zh ops`
commands you’ll be prompted for the vault password.

### Local Overrides

If you want to override any ansible variables without having to edit configs,
you can put a value in `vars.yml`, which is in `.gitignore` so you can’t commit
it. This is a convenient way to develop new features before using the vault.


[Ansible]: http://docs.ansible.com
