-- migrate:up

create table tag_aliases (
  tag varchar not null,
  alias varchar not null,
  primary key (tag, alias)
);

-- migrate:down

drop table tag_aliases;