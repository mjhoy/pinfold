create table projects
(
  id serial     primary key,

  title         text not null,
  description   text not null,

  published     boolean not null default false,

  created       timestamp without time zone not null default (now() at time zone 'utc'),
  updated       timestamp without time zone not null default (now() at time zone 'utc')
);
