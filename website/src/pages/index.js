/**
 * Copyright (c) Meta Platforms, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React from 'react';
import clsx from 'clsx';
import Layout from '@theme/Layout';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import useBaseUrl from '@docusaurus/useBaseUrl';
import styles from './styles.module.css';

const featuresIntro = [
  {
    title: '',
    imageUrl: 'img/GeoLiftLogo.svg',
    description: (
      <>
      </>
    ),
  },
];

const features = [
  {
    title: 'Focus on True Business Outcomes',
    imageUrl: 'img/GeoLift_Grow.svg',
    description: (
      <>
        GeoLift is Facebook's open-source solution to measure Lift at
        a geographical level. By leveraging the latest advances in Synthetic
        Control Methods, GeoLift empowers you to start making decisions grounded
        in incrementality and measure the true value of your marketing campaigns.
      </>
    ),
  },
  {
    title: 'Reproducible & Transparent',
    imageUrl: 'img/GeoLift_Transparent.svg',
    description: (
      <>
        GeoLift offers an end-to-end solution to geo-experimentation which spans
        data ingestion, power analyses, market selection, and inference in an easy-
        to-use R package. Moreover, the tool's open-source nature makes it transparent
        and fully reproducible!
      </>
    ),
  },
  {
    title: 'Privacy Conscious & Resilient',
    imageUrl: 'img/GeoLift_Resilient.svg',
    description: (
      <>
        By relying exlcusively on aggregated market-level data and not personal
        information, GeoLift is resilient to signal loss all while protecting the
        consumer's privacy.
      </>
    ),
  },
];

function FeatureIntro({imageUrl, title, description}) {
  const imgUrl = useBaseUrl(imageUrl);
  return (
    <div className={clsx('col col--15', styles.feature)}>
      {imgUrl && (
        <div className="text--center">
          <img className={styles.featureImage} src={imgUrl} alt={title} />
        </div>
      )}
      <h3>{title}</h3>
      <p>{description}</p>
    </div>
  );
}

function Feature({imageUrl, title, description}) {
  const imgUrl = useBaseUrl(imageUrl);
  return (
    <div className={clsx('col col--4', styles.feature)}>
      {imgUrl && (
        <div className="text--center">
          <img className={styles.featureImage} src={imgUrl} alt={title} />
        </div>
      )}
      <h3>{title}</h3>
      <p>{description}</p>
    </div>
  );
}

export default function Home() {
  const context = useDocusaurusContext();
  const {siteConfig = {}} = context;
  return (
    <Layout
      title={`Hello from ${siteConfig.title}`}
      description="Description will go into a meta tag in <head />">
      <header className={clsx('hero hero--primary', styles.heroBanner)}>
        <div className="container">
          <h1 className="hero__title">{siteConfig.title}</h1>
          <p className="hero__subtitle">{siteConfig.tagline}</p>
          <div className={styles.buttons}>
            <Link
              className={clsx(
                'button button--secondary button--lg'
              )}
              to={useBaseUrl('docs/intro')}>
              Get Started
            </Link>
          </div>
        </div>
      </header>
      <main>

      <div className="padding-vert--xl">
          <div className="container">
            <div className="row">
              <div className={clsx('col col--6', styles.descriptionSection)}>
                <h2>Robust Geo-Experiments to Measure True Value</h2>
                <p className={styles.descriptionSectionText}>Changes to the ads ecosystem
                have made measuring the true value of marketing more challenging than before.
                Fortunately, geo-experiments remain one of the best ways to quantify ad
                effectiveness through lift, which we at Meta consider to be the gold standard
                of measurement. That's why we've developed GeoLift, an open-source tool that
                makes it easy to design and run robust geo-experiments! </p>
              </div>
              <div className="col col--6">
                <iframe
                  width="100%"
                  height="400"
                  src="https://www.youtube.com/embed/ogLGvNOpCmg"
                  frameborder="0"
                  allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
                  allowfullscreen
                />
              </div>
            </div>
          </div>
        </div>

        {features && features.length > 0 && (
          <section className={styles.features}>
            <div className="container">
              <div className="row">
                {features.map(({title, imageUrl, description}) => (
                  <Feature
                    key={title}
                    title={title}
                    imageUrl={imageUrl}
                    description={description}
                  />
                ))}
              </div>
            </div>
          </section>
        )}

        <div className="padding-vert--xl">
          <div className="container">
            <div className="row">
              <div className="col col--6">
                <iframe
                  width="100%"
                  height="400"
                  src="https://www.youtube.com/embed/RymwMzqrOc4"
                  frameborder="0"
                  allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
                  allowfullscreen
                />
              </div>
              <div className={clsx('col col--6', styles.descriptionSection)}>
                <h2>GeoLift: Simplicity in Geo-Experimentation</h2>
                <p className={styles.descriptionSectionText}>From identifying where to run your
                experiment and running the power calculations, to measuring the Lift from your
                marketing campaigns, GeoLift has the tools you need to understanding the true
                incremental value of your marketing and help businesses make better decisions. </p>
              </div>
            </div>
          </div>
        </div>

         {featuresIntro && featuresIntro.length > 0 && (
          <section className={styles.footerfeatures}>
            <div className="container">
              <div className="row">
                {featuresIntro.map(({title, imageUrl, description}) => (
                  <FeatureIntro
                    key={title}
                    title={title}
                    imageUrl={imageUrl}
                    description={description}
                  />
                ))}
              </div>
            </div>
          </section>
        )}
      </main>
    </Layout>
  );
}
