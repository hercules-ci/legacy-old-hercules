#include <algorithm>
#include <iostream>
#include <memory>
#include <sstream>
#include <string>

#include <nix/local-store.hh>
#include <nix/remote-store.hh>
#include <nix/store-api.hh>

namespace nix {

class HerculesStore : public Store {
public:
  ~HerculesStore(){};

  HerculesStore(const Store::Params &params,
                std::unique_ptr<Store> underlyingStore,
                const std::string &something)
      : Store(params), underlyingStore(std::move(underlyingStore)){};

  std::string getUri() override;
  PathSet queryAllValidPaths() override;
  void queryPathInfoUncached(
      const Path &path,
      std::function<void(std::shared_ptr<ValidPathInfo>)> success,
      std::function<void(std::exception_ptr exc)> failure) override;
  void queryReferrers(const Path &path, PathSet &referrers) override;
  PathSet queryDerivationOutputs(const Path &path) override;
  StringSet queryDerivationOutputNames(const Path &path) override;
  Path queryPathFromHashPart(const string &hashPart) override;
  void addToStore(const ValidPathInfo &info, const ref<std::string> &nar,
                  bool repair, bool dontCheckSigs,
                  std::shared_ptr<FSAccessor> accessor) override;
  Path addToStore(const string &name, const Path &srcPath, bool recursive,
                  HashType hashAlgo, PathFilter &filter, bool repair) override;
  Path addTextToStore(const string &name, const string &s,
                      const PathSet &references, bool repair) override;
  void narFromPath(const Path &path, Sink &sink) override;
  void buildPaths(const PathSet &paths, BuildMode buildMode) override;
  BuildResult buildDerivation(const Path &drvPath, const BasicDerivation &drv,
                              BuildMode buildMode) override;
  void ensurePath(const Path &path) override;
  void addTempRoot(const Path &path) override;
  void addIndirectRoot(const Path &path) override;
  Roots findRoots() override;
  void collectGarbage(const GCOptions &options, GCResults &results) override;
  ref<FSAccessor> getFSAccessor() override;
  void addSignatures(const Path &storePath, const StringSet &sigs) override;

private:
  [[noreturn]] void notImpl(const std::string &operation);

  std::unique_ptr<Store> underlyingStore;
};

void HerculesStore::notImpl(const std::string &operation) {
  std::cerr << operation << std::endl;
  throw Error(operation + " is not implemented for the Hercules Store");
}

#define NOT_IMPL                                                               \
  do {                                                                         \
    notImpl(__PRETTY_FUNCTION__);                                              \
  } while (0)

std::string HerculesStore::getUri() { NOT_IMPL; }

PathSet HerculesStore::queryAllValidPaths() { NOT_IMPL; }

void HerculesStore::queryPathInfoUncached(
    const Path &path,
    std::function<void(std::shared_ptr<ValidPathInfo>)> success,
    std::function<void(std::exception_ptr exc)> failure) {
  NOT_IMPL;
}

void HerculesStore::queryReferrers(const Path &path, PathSet &referrers) {
  NOT_IMPL;
}

PathSet HerculesStore::queryDerivationOutputs(const Path &path) { NOT_IMPL; }

StringSet HerculesStore::queryDerivationOutputNames(const Path &path) {
  NOT_IMPL;
}

Path HerculesStore::queryPathFromHashPart(const string &hashPart) { NOT_IMPL; }

void HerculesStore::addToStore(const ValidPathInfo &info,
                               const ref<std::string> &nar, bool repair,
                               bool dontCheckSigs,
                               std::shared_ptr<FSAccessor> accessor) {
  NOT_IMPL;
}

Path HerculesStore::addToStore(const string &name, const Path &srcPath,
                               bool recursive, HashType hashAlgo,
                               PathFilter &filter, bool repair) {
  return underlyingStore->addToStore(name, srcPath, recursive, hashAlgo, filter, repair);
}

Path HerculesStore::addTextToStore(const string &name, const string &s,
                                   const PathSet &references, bool repair) {
  return underlyingStore->addTextToStore(name, s, references, repair);
}

void HerculesStore::narFromPath(const Path &path, Sink &sink) { NOT_IMPL; }

void HerculesStore::buildPaths(const PathSet &paths, BuildMode buildMode) {
  NOT_IMPL;
}

BuildResult HerculesStore::buildDerivation(const Path &drvPath,
                                           const BasicDerivation &drv,
                                           BuildMode buildMode) {
  NOT_IMPL;
}

void HerculesStore::ensurePath(const Path &path) { NOT_IMPL; }

void HerculesStore::addTempRoot(const Path &path) { NOT_IMPL; }

void HerculesStore::addIndirectRoot(const Path &path) { NOT_IMPL; }

Roots HerculesStore::findRoots() { NOT_IMPL; }

void HerculesStore::collectGarbage(const GCOptions &options,
                                   GCResults &results) {
  NOT_IMPL;
}

ref<FSAccessor> HerculesStore::getFSAccessor() { NOT_IMPL; }

void HerculesStore::addSignatures(const Path &storePath,
                                  const StringSet &sigs) {
  NOT_IMPL;
}

////////////////////////////////////////////////////////////////////////////////
// Register the store
////////////////////////////////////////////////////////////////////////////////

std::unique_ptr<Store> makeUnderlyingStore(const std::string &resource,
                                           const Store::Params &params) {
  switch (getStoreType(resource, get(params, "state", settings.nixStateDir))) {
  case tDaemon:
    return std::make_unique<UDSRemoteStore>(params);
  case tLocal:
    return std::make_unique<LocalStore>(params);
  default:
    return nullptr;
  }
}

static RegisterStoreImplementation
regStore([](const std::string &uri,
            const Store::Params &params) -> std::shared_ptr<Store> {
  const std::string prefix = "hercules://";
  const std::string protocol(uri, 0, prefix.length());
  if (protocol != prefix) {
    return nullptr;
  }

  const std::string resource(uri, prefix.length());
  std::unique_ptr<Store> underlyingStore(makeUnderlyingStore(resource, params));

  return std::shared_ptr<Store>(
      std::make_shared<HerculesStore>(params, std::move(underlyingStore), ""));
});
};
