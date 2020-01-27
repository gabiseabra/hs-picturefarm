-- migrate:up

create table tag_aliases (
  tag varchar not null,
  alias varchar not null,
  PRIMARY KEY (tag, alias)
);

-- migrate:down

drop table tag_aliases;