-- migrate:up

create table picture_tags (
  picture_uuid uuid references pictures(uuid),
  tag varchar not null,
  PRIMARY KEY (picture_uuid, tag)
);

-- migrate:down

drop table picture_tags;