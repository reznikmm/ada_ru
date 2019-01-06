create table posts (
  id      varchar primary key,
  author  varchar not null,
  sent    varchar not null,
  parent  varchar,
  subject varchar not null);

create table post_lines (
  post    varchar not null references posts,
  pos     integer not null,
  quote   integer not null default 0,
  text    varchar,
  primary key (post, pos));

create table forums (
  id       integer primary key,
  sort_key integer not null,
  subject  varchar not null);

create table topics (
  starter  varchar primary key,
  forum    integer not null references forums,
  subject  varchar not null);

create table users (
  nickname varchar primary key,
  name     varchar not null,
  avatar   varchar,
  created timestamp default current_timestamp);

create table emails (
  email    varchar primary key,
  nickname varchar not null references users,
  main     char(1));

INSERT INTO forums VALUES(1,10,'Обсуждение языка Ада');
