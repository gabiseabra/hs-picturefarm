-- migrate:up

create table pictures (
  uuid uuid not null primary key default uuid_generate_v4(),
  file_name varchar not null,
  file_hash char(32) not null,
  url varchar not null,
  mime_type character varying not null,
  created_at timestamp with time zone default now() not null,
  updated_at timestamp with time zone default now() not null
);

-- migrate:down

drop table pictures;