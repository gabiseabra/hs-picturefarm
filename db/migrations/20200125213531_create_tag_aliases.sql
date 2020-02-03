-- migrate:up

create table tag_aliases (
  tag varchar not null,
  alias varchar not null,
  primary key (tag, alias),
  created_at timestamp(6) default now() not null,
  updated_at timestamp(6) default now() not null
);

-- migrate:down

drop table tag_aliases;