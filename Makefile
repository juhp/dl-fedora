stack-all:
	stack-nightly build
	@echo
	stack --resolver lts-16 build
	@echo
	stack --resolver lts-14 build
	@echo
	stack --resolver lts-13 --stack-yaml stack-lts.yaml build
	@echo
	stack --resolver lts-12 --stack-yaml stack-lts.yaml build
	@echo
	stack --resolver lts-11 --stack-yaml stack-lts.yaml build
	@echo
	stack --resolver lts-10 --stack-yaml stack-lts10.yaml build
#	@echo # needs semigroup <>
#	stack --resolver lts-9 --stack-yaml stack-lts9.yaml build
#	@echo
#	stack --resolver lts-8 --stack-yaml stack-lts10.yaml build
