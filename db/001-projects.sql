create table projects
(
  pid           serial primary key,

  title         text not null,
  description   text not null,

  published     boolean not null default false,

  created       timestamp without time zone not null default (now() at time zone 'utc'),
  updated       timestamp without time zone not null default (now() at time zone 'utc')
);

create table items
(
  iid           serial primary key,
  pid           integer not null references projects (pid),

  path          text not null,
  caption       text,

  created       timestamp without time zone not null default (now() at time zone 'utc'),
  updated       timestamp without time zone not null default (now() at time zone 'utc')
);
