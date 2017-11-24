create table posts (
  id      varchar primary key,
  author  varchar not null,
  sent    varchar not null,
  parent  varchar,
  subject varchar not null,
  text    varchar);

create table forums (
  id       integer primary key,
  sort_key integer not null,
  subject  varchar not null);

create table topics (
  id       integer primary key,
  forum    integer not null references forums,
  starter  varchar not null,
  subject  varchar not null);

create table users (
  nickname varchar primary key,
  avatar   varchar);

create table emails (
  nickname varchar not null references users,
  main     char(1),
  email    varchar not null);
