create table `repo_namespaces` (
  `repo_id` char(36) NOT NULL,
  `namespace` VARCHAR(254) NOT NULL,
  created_at DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  primary key(`repo_id`,`namespace`)
)
;

insert into repo_namespaces (repo_id, namespace)
select repo_id, 'default' from target_items
;
