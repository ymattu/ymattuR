#include <Rcpp.h>
#include <mecab.h>

using namespace Rcpp;
using namespace MeCab;

//' MeCab DF
//' @param str Text
//' @param tagger_opt Tagger
// [[Rcpp::export]]
DataFrame mecab_df(std::string str, std::string tagger_opt) {
  std::vector<std::string> surface, feature;
  MeCab::Tagger *tagger = MeCab::createTagger(tagger_opt.c_str());
  const MeCab::Node *node(tagger->parseToNode(str.c_str()));
  for (; node; node = node->next) {
    if (node->stat != MECAB_BOS_NODE & node->stat != MECAB_EOS_NODE) {
      surface.push_back(std::string(node->surface, node->length));
      feature.push_back(std::string(node->feature));
    }
  }
  delete tagger;
  return wrap(
    DataFrame::create(
      Named("surface") = surface,
      Named("feature") = feature
    )
  );
}
