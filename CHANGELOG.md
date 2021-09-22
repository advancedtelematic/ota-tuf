# Changelog
All notable changes to `garage-sign` will be documented in this file. 

[Download page](https://tuf-cli-releases.ota.here.com/index.html)

## Upcoming release


## v0.7.4
### Added
- Added `--force` parameter to `garage-sign targets upload` command.
- Added `--verifyIntegrity` parameter to `garage-sign targets add-uploaded` command.
- Added validation of `root.json` before pushing to server.
### Fixed
- Fixed error with wrong `Created Time` after the update of the existing software version.
#### [Download](https://tuf-cli-releases.ota.here.com/cli-0.7.4.tgz)

## v0.7.3
### Added
- Added ability to add the external signatures to `targets.json`. Added parameter `--signatures` to `garage-sign targets sign` command.
- Added ability to add the external signatures to `root.json`. Added parameters `--signatures` and `--old-root-alias` to `garage-sign root sign` command.
- Added commands to add (`garage-sign root targets-key add`) or remove (`garage-sign root targets-key remove`) target keys from `root.json`.
- Added `garage-sign` command to get unsigned targets (`garage-sign targets get-unsigned`).
- Added `garage-sign` command to get unsigned root (`garage-sign root get-unsigned`).
- Added `garage-sign` command to import the public key (`garage-sign user-keys importpub`)
- Added `garage-sign` commands to increase `root.json` (`garage-sign root increment-version`) and `targets.json` (`garage-sign targets increment-version`) version.
- Added ability to upload binary files larger than 3Gb.
### Changed
- Don't add new targets key when moving offline without a new root key.
#### [Download](https://tuf-cli-releases.ota.here.com/cli-0.7.3-2-g40bedac.tgz)

## v0.7.2
### Added
- Added support of Azure blob Storage
### Changed
- Updated jre version to 8u262-b10 
- Migrated from Auth+ to AWS Cognito
#### [Download](https://tuf-cli-releases.ota.here.com/cli-0.7.2-1-g51e2f5b.tgz)
