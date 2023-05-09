default: help

pg_init: env_PG_DATA_DIR ## Set up Postgres data directory
	initdb -E utf8 -U postgres -D $(PG_DATA_DIR) -A trust
.PHONY: pg_init

pg_start: env_PG_DATA_DIR env_PG_LOG_FILE ## Start Postgres server in the background
	pg_ctl start -D $(PG_DATA_DIR) -l $(PG_LOG_FILE) -o '-c log_connections=on -c log_disconnections=on -c log_statement=all -c jit=off -c default_transaction_isolation=serializable -c shared_buffers=444008kB -c effective_cache_size=888016kB -c maintenance_work_mem=64MB -c checkpoint_completion_target=0.9 -c wal_buffers=13872kB -c default_statistics_target=100 -c random_page_cost=4 -c work_mem=4MB -c min_wal_size=192MB -c max_wal_size=2GB'
.PHONY: pg_start

pg_stop: env_PG_DATA_DIR env_PG_LOG_FILE ## Stop Postgres database server
	pg_ctl stop -D $(PG_DATA_DIR) -l $(PG_LOG_FILE) -m smart
.PHONY: pg_stop

build: ## Build the Haskell code
	stack build --test --no-run-tests --bench --no-run-benchmarks --pedantic pg-conn-memory-use
.PHONY: build

run_producer: ## Run the notification producer application
	stack run producer
.PHONY: run_producer

run_consumer: ## Run the notification consumer application
	stack run consumer
.PHONY: run_consumer

help: ## Print help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
.PHONY: help

# This target enforces that the specified environment variable is set, e.g.
# env_FOO enforces that the 'FOO' environment variable is set. This can be used
# to require user-specified environment variables local to only those targets
# that require them. For more info, see https://stackoverflow.com/a/7367903.
env_%: ENV
	@ if [ -z '${${*}}' ]; then echo 'Environment variable $* not set.' && exit 1; fi
ENV:
.PHONY: ENV
