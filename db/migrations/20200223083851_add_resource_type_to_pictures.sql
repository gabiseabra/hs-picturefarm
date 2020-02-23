-- migrate:up

alter table pictures
  add resource_type varchar;

update pictures
  set resource_type = split_part(mime_type, '/', 1);

-- migrate:down

alter table pictures
  drop column resource_type;