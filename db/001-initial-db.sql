---------------------------------------------------------------
-- | auth_users
create table auth_users
(
  uid                         serial primary key,
  login                       text not null,
  email                       text,
  password                    text,
  activated_at                timestamp with time zone,
  suspended_at                timestamp with time zone,
  remember_token              text,
  login_count                 integer not null,
  failed_login_count          integer not null,
  locked_out_until            timestamp with time zone,
  current_login_at            timestamp with time zone,
  last_login_at               timestamp with time zone,
  current_login_ip            text,
  last_login_ip               text,
  created_at                  timestamp with time zone,
  updated_at                  timestamp with time zone,
  reset_token                 text,
  reset_requested_at          timestamp with time zone
);

alter table only auth_users
    add constraint auth_users_login_key unique (login);


---------------------------------------------------------------
-- | admin
create table admins
(
  aid           serial primary key,
  uid           integer not null references auth_users (uid)
);


---------------------------------------------------------------
-- | projects
create table projects
(
  pid           serial primary key,
  aid           integer not null references admins (aid),

  title         text not null,
  description   text not null,

  published     boolean not null default false,

  created       timestamp with time zone not null default (now() at time zone 'utc'),
  updated       timestamp with time zone not null default (now() at time zone 'utc')
);


---------------------------------------------------------------
-- | items
create table items
(
  iid           serial primary key,
  pid           integer not null references projects (pid),

  path          text not null,
  caption       text,

  created       timestamp with time zone not null default (now() at time zone 'utc'),
  updated       timestamp with time zone not null default (now() at time zone 'utc')
);

