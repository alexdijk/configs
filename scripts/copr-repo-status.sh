#!/usr/bin/env sh
dists=(38 39 rawhide)
dnf clean all > /dev/null
for repo in fedora stable{,-rc} mainline{-wo-mergew,} next; do
	[[ ${repo} =~ (fedora|next) ]] && unset repostring
	repostring="${repostring} --repofrompath=kvr-${repo},https://download.copr.fedorainfracloud.org/results/@kernel-vanilla/${repo}/fedora-\${distro}-x86_64/"
	for distro in ${dists[*]} ; do
  	  queryresult="$(eval dnf repoquery ${repostring} --disablerepo=* --enablerepo=kvr-* --latest-limit=1 -q kernel --arch x86_64 --qf '%{version}-%{release}')"
		  printf '%-20s %-10s %s\n' "${repo}" "${distro}" "${queryresult:-lookup failed}"
	done
done

