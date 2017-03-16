create table `generated_root_roles` (
  repo_id CHAR(36) NOT NULL PRIMARY KEY,
  expires_at DATETIME(3) NOT NULL,
  content TEXT NOT NULL,
  created_at DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
)
;

create table `root_signatures` (
  repo_id CHAR(36) NOT NULL,
  key_id VARCHAR(64) NOT NULL,
  method ENUM('rsassa-pss') NOT NULL,
  signature TEXT NOT NULL,
  created_at DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP,
  updated_at DATETIME(3) NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  PRIMARY KEY (repo_id, key_id)
)
;
