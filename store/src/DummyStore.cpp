#include <algorithm>
#include <iostream>
#include <memory>
#include <sstream>
#include <string>

#include <nix/local-store.hh>
#include <nix/remote-store.hh>
#include <nix/store-api.hh>

namespace nix {

// 'HerculesStore' behaves exactly the same as the BaseStore from which it
// inherits, with the exception that the 'buildPaths' method is implemented by
// an external server.
template <class BaseStore> class HerculesStore : public BaseStore {
public:
  ~HerculesStore(){};

  HerculesStore(const Store::Params &params, const std::string &something)
      : Store(params), BaseStore(params){};

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
};

template <class BaseStore>
void HerculesStore<BaseStore>::notImpl(const std::string &operation) {
  throw Error(operation + " is not implemented for the Hercules Store");
}

#define NOT_IMPL                                                               \
  do {                                                                         \
    notImpl(__PRETTY_FUNCTION__);                                              \
  } while (0)

template <class BaseStore> std::string HerculesStore<BaseStore>::getUri() {
  NOT_IMPL;
}

template <class BaseStore>
PathSet HerculesStore<BaseStore>::queryAllValidPaths() {
  NOT_IMPL;
}

template <class BaseStore>
void HerculesStore<BaseStore>::queryPathInfoUncached(
    const Path &path,
    std::function<void(std::shared_ptr<ValidPathInfo>)> success,
    std::function<void(std::exception_ptr exc)> failure) {
  return BaseStore::queryPathInfoUncached(path, success, failure);
}

template <class BaseStore>
void HerculesStore<BaseStore>::queryReferrers(const Path &path,
                                              PathSet &referrers) {
  NOT_IMPL;
}

template <class BaseStore>
PathSet HerculesStore<BaseStore>::queryDerivationOutputs(const Path &path) {
  NOT_IMPL;
}

template <class BaseStore>
StringSet
HerculesStore<BaseStore>::queryDerivationOutputNames(const Path &path) {
  NOT_IMPL;
}

template <class BaseStore>
Path HerculesStore<BaseStore>::queryPathFromHashPart(const string &hashPart) {
  NOT_IMPL;
}

template <class BaseStore>
void HerculesStore<BaseStore>::addToStore(
    const ValidPathInfo &info, const ref<std::string> &nar, bool repair,
    bool dontCheckSigs, std::shared_ptr<FSAccessor> accessor) {
  NOT_IMPL;
}

template <class BaseStore>
Path HerculesStore<BaseStore>::addToStore(const string &name,
                                          const Path &srcPath, bool recursive,
                                          HashType hashAlgo, PathFilter &filter,
                                          bool repair) {
  return BaseStore::addToStore(name, srcPath, recursive, hashAlgo, filter,
                               repair);
}

template <class BaseStore>
Path HerculesStore<BaseStore>::addTextToStore(const string &name,
                                              const string &s,
                                              const PathSet &references,
                                              bool repair) {
  return BaseStore::addTextToStore(name, s, references, repair);
}

template <class BaseStore>
void HerculesStore<BaseStore>::narFromPath(const Path &path, Sink &sink) {
  NOT_IMPL;
}

template <class BaseStore>
void HerculesStore<BaseStore>::buildPaths(const PathSet &paths,
                                          BuildMode buildMode) {
  std::cerr << "buildPaths called for" << std::endl;
  std::for_each(paths.begin(), paths.end(), [](const Path &path) {
    std::cerr << "  - " << path << std::endl;
  });

  return BaseStore::buildPaths(paths, buildMode);
}

template <class BaseStore>
BuildResult HerculesStore<BaseStore>::buildDerivation(
    const Path &drvPath, const BasicDerivation &drv, BuildMode buildMode) {
  NOT_IMPL;
}

template <class BaseStore>
void HerculesStore<BaseStore>::ensurePath(const Path &path) {
  return BaseStore::ensurePath(path);
}

template <class BaseStore>
void HerculesStore<BaseStore>::addTempRoot(const Path &path) {
  NOT_IMPL;
}

template <class BaseStore>
void HerculesStore<BaseStore>::addIndirectRoot(const Path &path) {
  return BaseStore::addIndirectRoot(path);
}

template <class BaseStore> Roots HerculesStore<BaseStore>::findRoots() {
  NOT_IMPL;
}

template <class BaseStore>
void HerculesStore<BaseStore>::collectGarbage(const GCOptions &options,
                                              GCResults &results) {
  NOT_IMPL;
}

template <class BaseStore>
ref<FSAccessor> HerculesStore<BaseStore>::getFSAccessor() {
  return BaseStore::getFSAccessor();
}

template <class BaseStore>
void HerculesStore<BaseStore>::addSignatures(const Path &storePath,
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

  switch (getStoreType(resource, get(params, "state", settings.nixStateDir))) {
  case tDaemon:
    return std::shared_ptr<Store>(
        std::make_shared<HerculesStore<UDSRemoteStore>>(params, ""));
  case tLocal:
    return std::shared_ptr<Store>(
        std::make_shared<HerculesStore<LocalStore>>(params, ""));
  default:
    return nullptr;
  }
});
};
