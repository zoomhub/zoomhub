# Ops

### Prerequisites

- Install [Ansible]: `brew install ansible`.
  We have tested our setup with Ansible 2.9.0.

## CircleCI setup

Set up the following [environment variables on CircleCI][circleci-env-vars]:

- `AWS_ACCESS_KEY_ID`
- `AWS_DEFAULT_REGION`
- `AWS_SECRET_ACCESS_KEY`
- `ZH_AWS_ACCOUNT_ID`
- `ZH_AWS_EB_PROJECT`
- `ZH_AWS_ECR_REPO`

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

### Local overrides

If you want to override any ansible variables without having to edit configs,
you can put a value in `vars.yml`, which is in `.gitignore` so you can’t commit
it. This is a convenient way to develop new features before using the vault.

[circleci-env-vars]: https://app.circleci.com/settings/project/github/zoomhub/zoomhub/environment-variables

## AWS

### Elastic Beanstalk

- Application logs: `tail -f /var/log/eb-docker/containers/eb-current-app/*.log`
- nginx configuration: `cat /etc/nginx/nginx.conf`
- nginx extensions: `cat /etc/nginx/conf.d/*.conf` (note `.conf` extension)
- cron logs: `sudo tail -f /var/log/cron`

#### Adding new environment

- Add new rule to RDS security groups.
