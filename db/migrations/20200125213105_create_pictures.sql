-- migrate:up

create extension if not exists "uuid-ossp" with schema public;

create table pictures (
  uuid uuid not null primary key default uuid_generate_v4(),
  file_name varchar not null,
  file_hash char(32) not null,
  url varchar not null,
  mime_type character varying not null,
  created_at timestamp(6) default now() not null,
  updated_at timestamp(6) default now() not null
);

create index pictures_updated_at_idx on pictures (updated_at desc);
create index pictures_file_name_idx on pictures (file_name);

-- migrate:down

drop table pictures;
